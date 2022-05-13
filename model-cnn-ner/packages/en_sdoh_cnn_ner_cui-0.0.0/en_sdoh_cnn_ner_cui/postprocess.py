import json
import logging

import spacy
from spacy.language import Language
from spacy.tokens import Span

Span.set_extension("cui", default=None, force=True)
Span.set_extension("snomed_ct", default=None, force=True)
Span.set_extension("canonical_text", default=None, force=True)

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
                logging.warning(f"cannot find an entry for {ent} ({ent.label_}) in {len(self.data)} entries")
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

    def initalize(self, get_examples=None, nlp=None, data={}):
        logging.info("initializing sdoh_cui")
        logging.info(f"data: {data}")
        self.data = data

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
