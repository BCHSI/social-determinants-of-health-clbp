# Models for extraction of Social Determinants of Health from clinical notes

- RoBERTa NER: [model-roberta-ner](model-roberta-ner)
- CNN NER: [model-cnn-ner](model-cnn-ner)
- Hybrid BoW-GBM NER: [model-hybrid-bow](model-hybrid-bow)

# Using models

To install a model, e.g. `model-hybrid-bow`, run following in a shell:

```sh
cd model-hybrid-bow
pip install package/
```

from within Python:

```python
import spacy
nlp = spacy.load("en_sdoh_bow")
text = "Home Environment:  Lives with family  Work: disability"
doc = nlp(text)

for ent in doc.ents:
    print(ent, ent.label_, ent._.score, ent._.cui, sep=" | ")
```

# References
Automatic Extraction of Social Determinants of Health from Medical Notes of Chronic Lower Back Pain Patients
Dmytro Lituiev, Benjamin Lacar,  Sang Pak, Peter L Abramowitsch, Emilia De Marchis,  Thomas Peterson

doi: [https://doi.org/10.1101/2022.03.04.22271541](https://doi.org/10.1101/2022.03.04.22271541)

