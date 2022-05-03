import spacy

class LemmaTokenizer():
    nlp = spacy.load("en_core_web_md")
    def __init__(self):
        pass
        
    def __call__(self, articles):
        doc = self.nlp(articles)
        result = [tok.lemma_ for tok in doc]
        # result = [self.wnl.lemmatize(t) for t in word_tokenize(articles)]
        return result

