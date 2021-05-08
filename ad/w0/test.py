div(n, d):
	r = n - d
	result = 0
	while r >= d:
		r = r - d
		result += 1

	return result

Invariante:
	n / d = (result * d) + r

Laufzeit mit Eingabegröße m:
3 + ( m * (floor(n / d)) )
