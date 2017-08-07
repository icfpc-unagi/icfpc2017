#include <stdio.h>
#include <stdlib.h>

int main() {
  setbuf(stdout, NULL);
  size_t bufsize = BUFSIZ;
  char* buf = malloc(bufsize);
  size_t n, m;
  do {
    if (scanf("%zu:", &n) < 1) break;
    if (bufsize < n) {
      buf = realloc(buf, n);
      bufsize = n;
    }
    m = fread(buf, 1, n, stdin);
    printf("%zu:", n + 1);
    fwrite(buf, 1, n, stdout);
    putchar('\n');
    fflush(stdout);
  } while (m == n);
}
