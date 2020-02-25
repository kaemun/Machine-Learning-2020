"""
Energy data NLP and prediction
------------------------------------------------------------------
Author: Simon (Hyungjun) Park

"""


"""
For the categorization of the predictions with natural language processing, we take the following steps:
    1. Remove unknown characters and classify each sentence according to energy type.
    2. Extract token related to target categories (e.g., "price") from each sentence.
    3. Conduct several methods of syntactic dependency parse. First, examine ancestors of each tokens.
    If we can't find any direction words, then check verb ancestors' children. Then, we can't find anything,
     try tokens' children.
    4. Compare the keywords (i.e.,ancestors or children of the verb or tokens' children) with direction words
     (e.g., "increase", "down" etc) to catch the predictions for each target and count the number of matched
      words. Also, we take negative sentence words into consideration.
For limitation, since we can't detect every dependency by simply using tokens and their ancestors or
children, some direction word are missed. Also, because of the shortage of reference about some energy type
(e.g., solar, wind), there are some NaNs in our final output dataframe.

"""

import PyPDF2
import pandas as pd
import requests
import os
import re
import spacy
import pyprind
import numpy as np

nlp = spacy.load("en_core_web_sm")


class ReadPDF():

    def __init__(self, years, path = os.getcwd()):
        self.years = years
        self.path = path
        self.textinpage = self.read_pdf()

    def get_pdf(self):
        """
        Download PDF files from EIA

        """
        if not any([os.path.exists(os.path.join(self.path,'aeo{}.pdf'.format(y))) for y in self.years]):
            print('Downloading data... (overwriting)')
            for y in self.years:
                dl_url = r'https://www.eia.gov/outlooks/aeo/pdf/aeo{}.pdf'.format(y)
                r = requests.get(dl_url)
                file = dl_url.split('/')[-1]
                with open(os.path.join(self.path, file), 'wb') as pfile:
                    pfile.write(r.content)
            print('PDF(s) for', self.years, 'downloaded in your working directory.')
        else:
            print('PDF(s) for', self.years, 'already exist(s) in your working directory.')

    def read_pdf(self):
        """
        Read the PDFs
        (Reference: https://pythonhosted.org/PyPDF2/)

        Returns
        ---------
        pdf_textinpage: dictionary
            The value is text data for each year from PDF.
            The key is year (i.e., 2019, 2018)

        """
        self.get_pdf()

        pdf_textinpage = {}
        for y in self.years:
            with open(os.path.join(self.path,'aeo{}.pdf'.format(y)), 'rb') as ifile:
                pdf = PyPDF2.PdfFileReader(ifile)

                print(y)
                print('Number of pages:', pdf.numPages)
                pbar = pyprind.ProgBar(pdf.numPages)

                pages = []
                for p in range(pdf.numPages):
                    page = pdf.getPage(p)
                    text = page.extractText()
                    pages.append(text)
                    pbar.update()
            pdf_textinpage[y] = pages
        print('\n')
        return pdf_textinpage



class ForecastPDF():

    def __init__(self, pdf_file, energy_dict, target_dict, predict_dict, pdf_index, start = None, end = None):
        self.pdf = pdf_file[pdf_index][start:end]
        self.pdf_index = pdf_index
        self.ene_dict = energy_dict
        self.trgt_dict = target_dict
        self.prdt_dict = predict_dict
        self.resultdf = self.df_builder()

    def cleaner(self, text):
        """
        Remove unknown characters and rearrage line break

        Input
        -------
        text: list
            text data for each year from PDF.

        Return
        -------
        text: list
            text data without unknown characters

        """
        text = re.sub('\n|Š','', text)
        text = re.sub('Ł',' ', text)
        return text

    def token_generator(self, target_words, sents):
        """
        Generate token according to the categories of target variable

        Input
        -------
        target_term: list
            words related to target term.
        sents: list
            Sentences mentioned energy type we are interested in

        Return
        -------
        tokens: list of spacy.tokens.token

        """
        tokens = []
        for s in sents:
            token =  [t for t in s if any([k in t.string for k in target_words])]
            tokens.extend(token)
        return tokens

    def detector(self, keys):
        """
        Detect whether related words appear in prediction word dictionary and count the number.
        If the list of keywords contain negative words (i.e., no or not), inverse the direction of
        prediction.

        Input
        -------
        keys: list
            Token's ancestors or children of token's ancestor or token's children.

        Return
        -------
        result: 1d-array

        """
        result = []
        words_list = [[w.lemma_.strip() for w in k] for k in keys]

        if any([[w for w in words if w in ['no', 'not']] for words in words_list]):
            direction = list(prdt_dict.keys())[::-1]
        else:
            direction = list(self.prdt_dict.keys())

        for d in direction:
            judge = [[w for w in words if w in self.prdt_dict[d]] for words in words_list]
            judge = [l for l in judge if len(l) > 0]
            result.append(len(judge))
        if all([j == 0 for j in result]):
            return True, None
        else:
            return False, np.array(result)

    def predicator(self, tokens):
        """
        Try several syntactic dependency parse method to find words related to target word

        Input
        -------
        tokens: list of spacy.tokens.token

        Return
        -------
        result: 1d-array

        """
        ancestors = [list(t.ancestors) for t in tokens]
        anc_flat = [a for anc in ancestors for a in anc]

        key_words_set = [ ancestors,
                         [list(a.children) for a in anc_flat if a.pos_ == 'VERB'],
                         [list(t.children) for t in tokens]]
        for keys in key_words_set:
            trial = self.detector(keys)
            if trial[0]:
                continue
            else:
                return trial[1]
                break
        return np.array([0,0,0])

    def classifier(self, document, tg):
        """
        Integrate all functions to summarize the document

        Input
        -------
        document: text data
            Scraping from PDF
        tg: string
            target words

        Return
        -------
        results: array-like
            The matrix of precited direction for each energy type about specific targe term

        """
        results = np.zeros([len(self.ene_dict), len(self.prdt_dict)])
        for i, page in enumerate(document):
            text = self.cleaner(page)
            doc = nlp(text)

            type_sents = {}
            for e in self.ene_dict.keys():
                type_sents[e] = [sent for sent in doc.sents if any([e in sent.string for e in self.ene_dict[e]])]

            by_page = np.zeros((0, len(self.prdt_dict)))
            for e in type_sents.keys():
                tokens = self.token_generator(self.trgt_dict[tg], type_sents[e])
                by_sent = self.predicator(tokens)
                by_page =  np.r_[by_page, by_sent.reshape(1,-1)]

            results += by_page
        return results

    def df_builder(self):
        """
        Aggregate the results of classification and generate dataframe

        Return
        ------
        final_output: Data frame

        """
        final_output = pd.DataFrame(index = [])
        for tg in self.trgt_dict.keys():
            result_by_page = self.classifier(self.pdf, tg)
            df_by_target = pd.DataFrame(result_by_page, index = self.ene_dict.keys(), columns = self.prdt_dict.keys())
            result = df_by_target.apply(lambda x: "" if x.sum() == 0 else x.idxmax(axis = 1), axis = 1)
            final_output = pd.concat([final_output, result], axis=1, sort=False)
        final_output.columns = self.trgt_dict.keys()
        print('File:', self.pdf_index)
        return  print(final_output, '\n')

'''
Set Parameters
'''

years = [2019,2018]
path = os.path.dirname(os.path.realpath(__file__)) #Use os.getcwd() instead if not running the whole script

trgt_dict = {'Price'    :['price','cost'],
             'Emissions':['emission', 'emit','CO2'],
             'Produce'  :['production', 'produce', 'yield', 'generate', 'generation'],
             'Trade'    :['export', 'import', 'exporter', 'importer','trade']}

prdt_dict = {'Up'      : ['increase', 'increasingly', 'up','upper' 'high','raise', 'grow','growth','rise'],
             'Unchange': ['unchange', 'stable', 'same'],
             'Down'    : ['decrease','decline','down', 'low','lower','loss', 'fall','reduce']}

ene_dict = {'Coal'    :['coal'],
            'Nuclear' :['nuclear'],
            'Wind'    :['wind'],
            'Solar'   :['solar'],
            'Oil'     :['oil','petroleum']}

'''
Execute

'''

rawpdfs = ReadPDF(years, path)
pdfs = rawpdfs.textinpage

forecast19 = ForecastPDF(pdfs, ene_dict, trgt_dict, prdt_dict, pdf_index=2019, start=5, end=13)
forecast18 = ForecastPDF(pdfs, ene_dict, trgt_dict, prdt_dict, pdf_index=2018, start=2, end=15)
