import os

for i in range(12):
    for j in range(2): # 2nd and 3rd run
        # print('sbatch --partition=west -o out/partdiff-{}c-%A.out -N1 -n1 timescript {}'.format(i+1, i+1))
        os.system('sbatch --partition=west -o out/partdiff-{}c-{}-%A.out -N1 -n1 timescript {}'.format(i+1, j+1, i+1))
