import sys
import spacy
import yaml
import re
from spacy.lang.en import English
from spacy.util import get_lang_class

@spacy.registry.callbacks("customize_tokenizer_pathology")
def make_customize_tokenizer_pathology():
    def customize_tokenizer(nlp):
        del spacy.lang.tokenizer_exceptions.BASE_EXCEPTIONS["8)"]
        with open('special-cases-spacy.yaml') as fh:
            special_cases = yaml.safe_load(fh,)
        
        tokenizer = nlp.tokenizer
        tokenizer.rules = {key: value for key, value in nlp.tokenizer.rules.items() if key != "8)"}
        for case in special_cases:
            tokenizer.add_special_case(*case)

        infixes = nlp.Defaults.infixes + [
                                  r'''[;,:]''',
                                  r'(?<=[a-zA-Z_])[\.\-\)]', 
                                  r'(?<=\.\-\))[a-zA-Z_]', 
                                  r'[\.^](?=[a-zA-Z_\-])',
                                  r'[\~](?=[\d]+)',
#                                   r'(?=\d+\.?\d+)([a-z])',
                                  r'\=',
#                                   r'(?<=[a-z])[\.^]',  r'[\.^](?=[a-z])',
#                                   r'(?<=[a-zA-Z_])[\.\(\)/](?=\d+)',
                                  r'(?<=[a-zA-Z_])[\.\(\)/](?=[a-zA-Z_0-9])', 
                                  ]
        
        infix_regex = spacy.util.compile_infix_regex(infixes)
        tokenizer.infix_finditer = infix_regex.finditer

        suffixes = nlp.Defaults.suffixes + [r'''\.''',
                                    r"\(\d+", r'(?<=[/\.^])[a-zA-Z_\-]+',
                                    r'/', 
                                    r"\++",
                                   ]
        
        suffix_regex = spacy.util.compile_suffix_regex(suffixes)
        tokenizer.suffix_search = suffix_regex.search

        prefixes = nlp.Defaults.prefixes + [
                                            r'''-''',
                                            r"/", # '\.',
                                            r'~', 
                                            r'''^[±\-\+0-9., ]+[0-9 ]+''',
                                            r'''^[±\-\+., ]*[0-9]+(\.[0-9]+)?''' 
                                           ]
        prefix_regex = spacy.util.compile_prefix_regex(prefixes)
        tokenizer.prefix_search = prefix_regex.search

        tokenizer.url_match = None
        nlp.tokenizer = tokenizer
        print("tokenizer customized")
        return nlp
    return customize_tokenizer 


@spacy.registry.callbacks("customize_tokenizer")
def make_customize_tokenizer():
    def customize_tokenizer(nlp):
        custom_nlp = spacy.load("en_core_web_sm_pathology")
        # quickest way to copy all the settings from one tokenizer to another
        nlp.tokenizer.from_bytes(custom_nlp.tokenizer.to_bytes())
        return nlp
    return customize_tokenizer
