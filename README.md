# Models for extraction of Social Determinants of Health from clinical notes

- RoBERTa NER: [model-roberta-ner](model-roberta-ner)
- CNN NER: [model-cnn-ner](model-cnn-ner)
- Hybrid BoW-GBM NER: [model-hybrid-bow](model-hybrid-bow)

# Using models

It is advised to create a virtual environment for this package, e.g.:
`conda create -n sdoh python3.9 spacy`

To install a model, e.g. `model-hybrid-bow`, run following in a shell:

```sh
pip install sklearn
python -m spacy download en_core_web_md

cd model-hybrid-bow/package/en_sdoh_bow_cui-0.0.2 && pip install . && cd -
# OR
cd model-cnn-ner/packages/en_sdoh_cnn_ner_cui-0.0.0 && pip install . && cd -
```

from within Python:

```python
import spacy
nlp = spacy.load("en_sdoh_bow_cui")
text = "Home Environment:  Lives with family  Work: disability"
doc = nlp(text)

for ent in doc.ents:
    print(ent, ent.label_, ent._.score, ent._.cui, sep=" | ")
```

# References
Automatic Extraction of Social Determinants of Health from Medical Notes of Chronic Lower Back Pain Patients
Dmytro Lituiev, Benjamin Lacar,  Sang Pak, Peter L Abramowitsch, Emilia De Marchis,  Thomas Peterson

doi: [https://doi.org/10.1101/2022.03.04.22271541](https://doi.org/10.1101/2022.03.04.22271541)

