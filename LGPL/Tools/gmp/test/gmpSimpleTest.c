#include <stdio.h>
#include <gmp.h>

int main (int argc, char **argv)
{
	mpz_t a, b, p;

	if (argc != 3)
	{
		printf ("Usage: %s <number> <number>\n", argv[0]);
		return 1;
	}

	/* Initialize and assign a and b from base 10 strings in argv */
	mpz_init_set_str (a, argv[1], 10);
	mpz_init_set_str (b, argv[2], 10);
	/* Initialize p */
	mpz_init (p);

	/* Multiply a and b and put the result in p */
	mpz_mul (p, a, b);

	/* Print p in decimal */
	gmp_printf ("Product is: %Zd\n", p);

	/* Since we're about to exit, no need to clear out variables */
	return 0;
}
