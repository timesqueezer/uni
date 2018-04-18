#!/usr/bin/env python

import hashlib

user = 'berta'
salt = 'xohth4dew5p8'
pw_hash = '14146888a9cb5e924987691876fb4252'

with open('deutsch.txt') as dict_file:
    for word in dict_file:
        if len(word) <= 7: # take newline into account
            word_without_newline = word[0:-1].lower() # stripe newline and make lowercase
            bytes = (salt + word_without_newline).encode('utf-8')
            test_hash = hashlib.md5(bytes).hexdigest()

            if test_hash == pw_hash:
                print('PWNED:', word_without_newline)
