# include <stdio.h>

extern int read () {
  int d;
  printf ("> ");
  scanf ("%d", &d);
  return d;
}

extern void write (int x) {
  printf ("%d\n", x);
}
