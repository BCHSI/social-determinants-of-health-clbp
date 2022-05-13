#!/usr/bin/env python
# coding: utf-8

import pandas as pd
import json
import logging
import os
from pathlib import Path

import spacy
from spacy.language import Language
from spacy.tokens import Span
from pydantic import StrictStr

Span.set_extension("cui", default=None)
Span.set_extension("snomed_ct", default=None)
Span.set_extension("canonical_text", default=None)


@Language.factory("sdoh_cui")
class SDOH:
    def __init__(self, nlp: Language, name: str = "sdoh_cui", 
                 #log_level: StrictStr
                ):
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
    
    def ainput_dir(self, key, value):
        # Ainput_dir something to the component's data
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

# input_dir = "spacy-level-2/sdoh-best/"
input_dir = "checkpoints-sdoh-2022-01-27-lr-0.0002-final/model-best/"
# input_dir = "packages/en_sdoh-0.0.0/"
# input_dir = "packages/en_sdoh-0.0.0/dist/en_sdoh-0.0.0/en_sdoh/en_sdoh-0.0.0"
output_dir = "sdoh-with-cuis"

os.makedirs(f"{output_dir}/sdoh_cui", exist_ok=True)

nlp = spacy.load(input_dir)
nlp.meta.update({"name": "sdoh_cnn_cui",
  "spacy_version":">=3.2.1,<3.3.0",
  "description":"CNN based Social determinants of health NER with a fine-grained window context bag-of-words classifier and a standard ontology extension interface (SNOMED_CT and CUI)",
  "author":"Dima Lituiev",
  "email": "d.lituiev@gmail.com"
})

sdoh_cui = SDOH(nlp)
sdoh_cui.from_disk(Path("model/"))


nlp.add_pipe("sdoh_cui")
nlp.pipeline[2][1].from_disk(Path("model/"))


nlp.to_disk(output_dir)

text = "He lives with wife in a studio."
doc = nlp(text)

for ent in doc.ents:
    print(ent, ent.label_, ent._.cui,  ent._.canonical_text, sep = " | ")


list(nlp("a (1/8). "))
