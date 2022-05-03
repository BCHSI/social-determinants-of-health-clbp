#!/usr/bin/env python
# coding: utf-8

import pickle
import sklearn
import numpy as np
import json

import spacy
from spacy.matcher import Matcher
from spacy.tokens import Span
from spacy.language import Language
from spacy.util import filter_spans

from typing import Any, Callable, Dict, Iterable, List, Optional, Tuple

# import lemma_tokenizer
import logging

Span.set_extension("score", default=None, force=True)
Span.set_extension("cui", default=None)
Span.set_extension("snomed_ct", default=None)
Span.set_extension("canonical_text", default=None)

class LemmaTokenizer():
    nlp = spacy.load("en_core_web_md")
    def __init__(self):
        pass
        
    def __call__(self, articles):
        doc = self.nlp(articles)
        result = [tok.lemma_ for tok in doc]
        # result = [self.wnl.lemmatize(t) for t in word_tokenize(articles)]
        return result

#import sys
#print("file:", __file__, file=sys.stderr)
#import ipdb; ipdb.set_trace()

@Language.factory("sdoh_cui")
class SDOH:
    def __init__(self, nlp, name="sdoh_cui"):
        self.data = {}

    def __call__(self, doc):
        for ent in doc.ents:
            entry = self.data.get(ent.label_, None)
            if entry is not None:
                cui = entry.get("CUI", None)
                if cui:
                    ent._.set("cui", cui)
                snomed_ct = entry.get("SNOMED_CT", None)
                if snomed_ct:
                    ent._.set("snomed_ct", snomed_ct)
                canonical_text = entry.get("canonical_text", None)
                if snomed_ct:
                    ent._.set("canonical_text", canonical_text)
            else:
                logging.warning(f"cannot find an entry for {ent} ({ent.label_})")
        return doc
    
    def add(self, key, value):
        # Add something to the component's data
        self.data[key] = value

    def to_disk(self, path, exclude=tuple()):
        # This will receive the directory path + /my_component
        data_path = path / "mapping-ontology-to-umls.json"
        with data_path.open("w", encoding="utf8") as f:
            f.write(json.dumps(self.data))

    def from_disk(self, path, exclude=tuple()):
        # This will receive the directory path + /my_component
        data_path = path / "mapping-ontology-to-umls.json"
        with data_path.open("r", encoding="utf8") as f:
            self.data = json.load(f)
        return self

# with open("./mapping-ontology-to-umls.json") as fh:
#     meta = json.load(fh)
#     
# 
# @Language.component("sdoh_cui")
# def add_sdoh_cui(doc):
#     for ent in doc.ents:
#         entry = meta.get(ent.label_, None)
# 
#         if entry is not None:
#             cui = entry.get("CUI", None)
#             if cui:
#                 ent._.set("cui", cui)
#             snomed_ct = entry.get("SNOMED_CT", None)
#             if snomed_ct:
#                 ent._.set("snomed_ct", snomed_ct)
#             canonical_text = entry.get("canonical_text", None)
#             if snomed_ct:
#                 ent._.set("canonical_text", canonical_text)
#         else:
#             logging.warning(f"cannot find an entry for {ent} ({ent.label_})")
#     return doc
# 

class LemmaTokenizer():
    nlp = spacy.load("en_core_web_md")
    def __init__(self):
        return self
        
    def __call__(self, articles):
        doc = self.nlp(articles)
        result = [tok.lemma_ for tok in doc]
        # result = [self.wnl.lemmatize(t) for t in word_tokenize(articles)]
        return result

# spacy.__main__.LemmaTokenizer = LemmaTokenizer

def initalize_matcher(path, nlp = spacy.load("en_core_web_md")):
    with open(path) as fh:
        domain_patterns_imp = json.load(fh)
    #nlp.add_pipe('sentencizer')
    matcher = Matcher(nlp.vocab)
    for domain in domain_patterns_imp:
        matcher.add(domain, domain_patterns_imp[domain])
    return matcher


def get_snippet(ent, margin=4):
    return ent.doc[max(0, ent.start-margin):min(len(ent.doc), ent.end+margin)]

def read_classification_models(path,
            level1 = ['Housing', 'Marital_or_partnership_status', 'Depression', 
              'Social_isolation', 'pain_and_disability', 'Anxiety', 
              'Insurance_status', 'Transportation', 'Financial_strain']):
    classification_models = {}
    for lbl in level1:
        with open(f"{path}/logistic-regression-{lbl}.pickle", "rb") as fh:
            model = pickle.load(fh)
        model.classes_ = [{"non-specific": None, "manual_NA": "NA"}.get(x,x) for x in model.classes_]
        classification_models[lbl] = model
    return classification_models

def write_classification_models(path, models):
    for lbl, mdl in models.items():
        with open(f"{path}/logistic-regression-{lbl}.pickle", "wb") as fh:
            pickle.dump(mdl, fh)
    return


@Language.factory("sdoh_bow")
class Level2Predictor():

    def __init__(self, nlp, name="sdoh_level2"):
        self.nlp = nlp
        return

    def from_disk(self, path, exclude=tuple()):
        path_matcher = path / "domain_patterns_dict_manedit.json"
        path_tfidf = path / "tfidf.pickle"
        self.matcher = initalize_matcher(path_matcher, self.nlp)
        with open(path_tfidf, "rb") as fh:
            self.tfidf = pickle.load(fh)
        self.classification_models = read_classification_models(path)

    def to_disk(self, path, exclude=tuple()):
        path_matcher = path / "domain_patterns_dict_manedit.json"
        path_tfidf = path / "tfidf.pickle"

        matcher_patterns = {self.matcher.vocab.strings[key]:val 
                            for key, val in self.matcher._patterns.items()}
        with open(path_matcher, "w") as fh:
            json.dump(matcher_patterns, fh)

        with open(path_tfidf, "wb") as fh:
            pickle.dump(self.tfidf, fh)
        write_classification_models(path, self.classification_models)


    def initialize(
        self,
        get_examples: Callable[[], Iterable[Example]],
        path,
        nlp: Optional[Language] = None,
        ):
        
        self.from_disk(path)
            

    
    def __call__(self, doc):
        matches = self.matcher(doc)
        spans = [Span(doc, start, end, label=match_hash) for match_hash, start, end in matches]
        spans = filter_spans(spans)
        spans = self.predict_spans(spans)
        doc.set_ents(spans)
        return doc
        
    def predict_spans(self, spans):
        if len(spans)==0:
            return spans
        features = self.tfidf.transform([str(get_snippet(ent)) for ent in spans])
        new_spans = []
        for ii, ent in enumerate(spans):
            if ent.label_ in self.classification_models:
                pr = self.classification_models[ent.label_].predict_proba(features[ii, :])
                idx = pr.argmax()
                lvl2 = self.classification_models[ent.label_].classes_[idx]
                lvl2_prob = pr.max()
                if lvl2 is not None:
                    ent.label_ += ": " + lvl2
                    ent._.set("score", lvl2_prob)
                    new_spans.append(ent)
            else:
                ent.label_ += ": NA"
                new_spans.append(ent)
        return new_spans


if __name__ == "__main__":
    predictor = Level2Predictor(spacy.load("en_core_web_md"))

    nlp = spacy.load("en_core_web_md")

    nlp.add_pipe("sdoh_level2")
    text = "Lives with wife in Berkely Hills. Complains about traffic"
    doc = nlp(text)
    for ent in doc.ents:
        print(ent, ent.label_, ent._.score, sep=" | ")
