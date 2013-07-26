
/* Algorithm E. (Date of Easter)
 * In "The Art of Computer Programming, Volume 1, Fundamental Algorithms"
 * Donald E. Knuth (1969), pages 155-156
 */
void getEaster(int *y, int *m, int *d, int *n)
{
  int i, Y, G, C, X, Z, D, E, N;
  for (i = 0; i < *n; i++) {
    Y = y[i];
    G = Y % 19 + 1;                 // Golden number
    C = Y / 100 + 1;                // Century
    X = (3 * C) / 4 - 12;           // Corrections
    Z = (8 * C + 5) / 25 - 5;
    D = (5 * Y) / 4 - X - 10;       // Find Sunday
    E = (11 * G + 20 + Z - X) % 30; // Epac
    N = 44 - E;                     // Full moon
    N += (N < 21) ? 30 : 0;
    N += 7 - (D + N) % 7;           // Advance to Sunday
    m[i] = (N > 31) ? 4 : 3;          // Get month
    d[i] = (N > 31) ? N - 31 : N;     // Get day    
  }
}

/* Example of R wraper function:
getEaster <- function(y) {
  if (!is.loaded("getEaster")) {
    dyn.load(file.path(paste("C/misc_hours", .Platform$dynlib.ext, sep="")))
    cat(" -Loaded ", "misc_hours","\n")
  }
  n <- NROW(y)
  m <- rep(1L, n)
  d <- rep(1L, n)
  ans <- .C("getEaster", y = as.integer(y), m = as.integer(m), d = as.integer(d), n = as.integer(n))
  data.frame(year = ans$y, month = ans$m, day = ans$d)
}
*/
