This is a Blackjack game I have written in Fortran. You can play so long as you still have chips in your wallet. If you say you've never played before at the beginning, you will receive 200 chips to start off with. Otherwise, it will go off the number inside your wallet file. You may continue playing rounds until you decide to quit or run out of chips to bet.

The initial project (full functionality for a standard game, no options to split or double down yet, cards displayed as letters when showing player and dealer's hand) was all done in the span of 8 hours. 

To run it you must have gfortran installed and compile the program by navigating to the directory where it's installed and using "gfortran blackjack.f95 -o a.exe" in the MSYS2 terminal. To run, enter "./a.exe" in the same terminal. I will likely make a standalone exe for it soon.
