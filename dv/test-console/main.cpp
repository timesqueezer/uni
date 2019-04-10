#include <QCoreApplication>
#include <iostream>
// #include <iomanip>
#include "flowdatasource.h"

#define N_SIZE 16
#define T_MAX 100

int main(int argc, char *argv[])
{
    // QCoreApplication a(argc, argv);

    // std::cout << std::setprecision(1);
    std::cout.precision(2);

    FlowDataSource source;
    source.createData(N_SIZE, 0);

    std::cout << "Value of plane z=4" << std::endl;
    source.printValuesOfHorizontalSlice(4);
    std::cout << "Value at last row: ";
    for (int i = 0; i < N_SIZE; ++i) {
        std::cout << source.getDataValue(i, N_SIZE - 1, N_SIZE - 1, 2) << ' ';
    }
    std::cout << std::endl;

    for (int i = 0; i < 3; ++i) {
        std::cout << "Max value for component " << i << ": " << source.getMaxValueOfComponent(i) << std::endl;
    }

    std::cout << "Lengths of last row: ";
    for (int i = 0; i < N_SIZE; ++i) {
        std::cout << source.getLength(i, N_SIZE - 1, N_SIZE - 1) << ' ';
    }
    std::cout << std::endl;

    std::cout << "Min/Max velocity" << std::endl;
    for (int t = 0; t < T_MAX; ++t) {
        source.createData(N_SIZE, t);
        std::cout << "t=" << t << ": ";

        std::cout << source.getMaxLength() << std::endl;
    }


    // return a.exec();
}
