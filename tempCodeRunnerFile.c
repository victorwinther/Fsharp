#include <stdio.h>

int main()
{
char *p;
char text[] = "This is the 02132 exam!";
p = &text[4];
p += 8;
printf("%s", p);
return 0;
}