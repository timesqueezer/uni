#!/usr/bin/env python

from pprint import pprint

"""text = 'GLS_EGFGGKLITDNE_REOUC_E_NDUHNEHDRD_ENENC_E_IANDNTKIRDNU___E'

for bigram in ['EN', 'ER']:
	first_letter, second_letter = list(bigram)

	results = {}

	print('Checking for ', bigram)

	for i, letter in enumerate(text):
		if letter == first_letter:
			# print('found first letter. checking text[i:]', i, text[i:])
			for j, other_letter in enumerate(text[i:]):
				k = j + i
				if other_letter == second_letter and k - i <= 15:
					# print('found at positions: ', i, k, ' with distance: ', k - i)
					if not results.get(k - i):
						results[k - i] = 1
					else:
						results[k - i] += 1

	pprint(results)"""


text = [3, 25, 2, 5, 18, 27]

p = 29
a = 2
y_b = 4
z = 5

for t in text:
	c = (pow(y_b, z) * t) % p
	print(c)

Integrität
Vertraulichkeit
Verfügbarkeit
Safety
Security
symmetrischer
asymmetrischer
hybrider
symmetrischen
asymmetrischen
hybriden
Message Authentication Code
Signatur
MAC