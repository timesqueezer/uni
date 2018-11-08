import os

for i in range(12):
    print('sbatch --partition=west -o out/partdiff-{}c-%A.out -N1 -n1 timescript {} 512'.format(i+1, i+1))
    # os.system('sbatch --partition=west -o partdiff-{}c-%A.out -N1 -n1 timescript {} 512'.format(i+1, i+1))

for i in range(11):
    interlines = 2**i
    print('sbatch --partition=west -o out/partdiff-{}i-%A.out -N1 -n1 timescript 12 {}'.format(interlines, interlines))
    # os.system('sbatch --partition=west -o partdiff-{}i-%A.out -N1 -n1 timescript 12 {}'.format(interlines, interlines))
