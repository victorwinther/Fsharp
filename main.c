#include <stdio.h>
int main()
{
dot_product(5,5,3);
}

double dot_product(double *a, double *b, int n)
{
double res = 0;
while (n > 0) {
res += a[n] * b[n];
n--;
}
return res;
}
