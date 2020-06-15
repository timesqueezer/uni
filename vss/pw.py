#!/usr/bin/env python

from hashlib import sha3_256

data = [
    {
        'name': 'DonaldChicken',
        'salt': 'SuperLongSpecialSalt',
        'hash': '63e6bd0c8348f7113400ccb5d26df0565dc56c447905ed05fbc80094d5320af0',
    },

    {
        'name': 'Penny',
        'salt': 'AlwaysUseADifferentSalt',
        'hash':'b998c2aeb78ed8948976a7c8e3317b16ba8a7ad45764520a672c684929f51f90',
    },

    {
        'name': 'Lulu',
        'salt': 'WowThisSaltingIsBrilliant',
        'hash':'aa24f11386c683e7536e3052ad2928503655497643b1572f9af08e2a605e0141',
    },

    {
        'name': 'Dagobart',
        'salt': 'OhNoHeDidnt',
        'hash': '7e80081b46537ce6c78846db4acd7e67944df8082131c637d6a4ed4d2215160c',
    },

    {
        'name': 'Dog',
        'salt': 'SuchWowSalt',
        'hash': 'dcdf36dbf3aa9a98b25e8f5814dcede3af2df3ddbc96c869fcb29cb2e0e7de9f',
    },
]


if __name__ == '__main__':
    found_names = []
    with open('pw.txt', 'r') as f:
        for pw in f:
            pw = pw.replace('\n', '')
            for d in data:
                if d['name'] in found_names:
                    continue

                salt_pw = ('{}{}'.format(d['salt'], pw)).encode('utf-8')
                hash = sha3_256()
                hash.update(salt_pw)
                hex_hash = hash.hexdigest()
                if hex_hash == d['hash']:
                    print('FOUND', d['name'], pw)
                    found_names.append(d['name'])
