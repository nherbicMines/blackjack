!--------------------------------------------------------------------------
! TITLE: Blackjack Simulator
! AUTHOR: Nicholas Herbic
! CLASS: CSCI260A
! DATE WRITTEN: 12/1/2021
! LAST REVISION: 12/8/2021
! DESCRIPTION: Plays a game of Blackjack with the user
!---------------------------------------------------------------------------

MODULE cards
IMPLICIT NONE
CHARACTER(LEN=2), DIMENSION(52), SAVE :: baseDeck = ['A.', '2.', '3.', '4.', '5.', '6.', '7.', '8.', '9.', '10', 'J.', 'Q.', 'K.', &
											'A.', '2.', '3.', '4.', '5.', '6.', '7.', '8.', '9.', '10', 'J.', 'Q.', 'K.', &
											'A.', '2.', '3.', '4.', '5.', '6.', '7.', '8.', '9.', '10', 'J.', 'Q.', 'K.', &
											'A.', '2.', '3.', '4.', '5.', '6.', '7.', '8.', '9.', '10', 'J.', 'Q.', 'K.']

CONTAINS
	!subroutine to shuffle the deck
	SUBROUTINE shuffle(deck)
	! Shuffles the baseDeck using Fisher-Yates algorithm
	! VARIABLES USED:
! 	NAME: 					TYPE:     							COMMENT:
!	baseDeck				CHARACTER(LEN=2), DIMENSION(52)		A baseline 1D array containing all 52 cards
!	deck					CHARACTER(LEN=2), DIMENSION(52)		The array where the shuffled deck is storesd
!	i						INTEGER								loop variable that steps through 51 cards
!	j						INTEGER								stores a random position in deck
!	x						REAL								stores random number to be transformed
!----------------------------------------------------------------------------------------------------------------
		CHARACTER(LEN=2), DIMENSION(52), INTENT(INOUT) :: deck
		INTEGER :: i, j
		REAL :: x
		
		deck = baseDeck
		
		DO i = 52, 2, -1
			CALL RANDOM_NUMBER(x)
			j = INT(x * 52) + 1
			
			CALL swap(deck(i), deck(j))
		END DO
		
			
	END SUBROUTINE shuffle
	
	
	SUBROUTINE swap(card1, card2)
!	Helper procedure for swapping cards in shuffle
!
! VARIABLES USED:
! 	NAME: 					TYPE:     							COMMENT:
!	card1					CHARACTER(LEN=2)					half of the pair being swapped
!	card2					CHARACTER(LEN=2)					other half of the pair being swapped
!	temp					CHARACTER(LEN=2)					temporarily stores card1 before it is swapped
!-----------------------------------------------------------------------------------------------------------------

		CHARACTER(LEN=2), INTENT(INOUT) :: card1, card2
		CHARACTER(LEN=2) :: temp
		
		temp = card1
		card1 = card2
		card2 = temp
	END SUBROUTINE swap
	
	!made a subroutine for this cause we're gonna be doing it a lot
	SUBROUTINE checkForEnd(deckPlace, deck)
!
!	Checks if we have reached the end of the deck every time a card is dealt
!
! VARIABLES USED:
! 	NAME: 					TYPE:     							COMMENT:
!	deck					CHARACTER(LEN=2), DIMENSION(52)		Deck used in main program and all subroutines
!	deckPlace				INTEGER								Rather than removing cards from array, this points to whatever card we are currently on
!-------------------------------------------------------------------------------------------------------------------------------------------------------

		CHARACTER(LEN=2), DIMENSION(52), INTENT(INOUT) :: deck
		INTEGER, INTENT(INOUT) :: deckPlace
		
		IF (deckPlace >= 52) THEN
			CALL shuffle(deck)
			deckPlace = 1
		END IF
	END SUBROUTINE checkForEnd
	
	!helper procedure for getting value of individual cards
	SUBROUTINE findValue(card, cardValue)
!
!	Defines values of integer arrays to each card using a select statement
!
! VARIABLES USED:
! 	NAME: 					TYPE:     							COMMENT:
!	card					CHARACTER(LEN=2)					The card to have its numerical value returned
!	cardValue				INTEGER, DIMENSION(:), ALLOCATABLE	Stores and returns the value of the card we're looking at. &
!																needs to be allocatable because of that pesky Ace that is 2 values at once.
!-----------------------------------------------------------------------------------------------------------------------------------------------
		CHARACTER(LEN=2), INTENT(IN) :: card
		INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: cardValue
		
		SELECT CASE (card)
			CASE ("A.")
				ALLOCATE(cardValue(2))
				cardValue = [1, 11]
			CASE ("2.")
				ALLOCATE(cardvalue(1))
				cardValue = [2]
			CASE ("3.")
				ALLOCATE(cardvalue(1))
				cardValue = [3]
			CASE ("4.")
				ALLOCATE(cardvalue(1))
				cardValue = [4]
			CASE ("5.")
				ALLOCATE(cardvalue(1))
				cardValue = [5]
			CASE ("6.")
				ALLOCATE(cardvalue(1))
				cardValue = [6]
			CASE ("7.")
				ALLOCATE(cardvalue(1))
				cardValue = [7]
			CASE ("8.")
				ALLOCATE(cardvalue(1))
				cardValue = [8]
			CASE ("9.")
				ALLOCATE(cardvalue(1))
				cardValue = [9]
			CASE ("10", "J.", "Q.", "K.")
				ALLOCATE(cardvalue(1))
				cardValue = [10]
		END SELECT
	END SUBROUTINE findValue
	
	!determines values of each player's hand
	SUBROUTINE determineValues(userHand, dealerHand, userIndex, dealerIndex, userValue, dealerValue)
!
!	Defines value of user & dealer's entire hand. Bit of a pain to make with the way the Ace works
!
! VARIABLES USED:
! 	NAME: 					TYPE:     							COMMENT:
!	userHand			CHARACTER(LEN=2), DIMENSION(26)			Card collection currently in user's hand
!	dealerHand			CHARACTER(LEN=2), DIMENSION(26)			^ but for the dealer
!	userValue			INTEGER, DIMENSION(:), ALLOCATABLE		Total numerical value of the user's hand
!	dealerValue			INTEGER, DIMENSION(:), ALLOCATABLE		^ but for the dealer
!	cardvalue			INTEGER, DIMENSION(:), ALLOCATABLE		Used for fetching a single card's value with findValue procedure
!	userIndex			INTEGER									Used as a count and indexer for user's hand
!	dealerIndex			INTEGER									^ but for the dealer
!	i					INTEGER									Stepper for do loop
!	temp				INTEGER									Stores a card's value while tiny card array is being reallocated
!	temp2				INTEGER									^ but for when we draw a second ace and need 2 places to store both of the old values
!-------------------------------------------------------------------------------------------------------------------------------------------------------
		CHARACTER(LEN=2), DIMENSION(26), INTENT(IN) :: userHand, dealerHand
		INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: userValue, dealerValue
		INTEGER, DIMENSION(:), ALLOCATABLE :: cardValue
		INTEGER, INTENT(IN) :: userIndex, dealerIndex
		INTEGER :: i, temp, temp2
		
		!Finds value of user's current hand
		DO i = 1, userIndex
			CALL findValue(userHand(i), cardValue)
			
			IF (SIZE(cardValue) == 2 .AND. ALLOCATED(userValue) .AND. SIZE(userValue) == 1) THEN	!If we draw an ace for first time (and userValue already allocated to 1)
				temp = userValue(1)
				DEALLOCATE(userValue)
				ALLOCATE(userValue(2))
				userValue(1) = temp + cardValue(1)
				userValue(2) = temp + cardValue(2)
			ELSE IF (SIZE(cardValue) == 2 .AND. ALLOCATED(userValue)) THEN	!If we draw an ace for the second time
			!This one was a bit of a headscratcher. Made me worried I might need some card arrays to be of size 4.
			!Realized that in a hand with two aces, the max + the max will always lead to a bust.
			!Therefore, we can maintain our size 2 card arrays but having one value be the Min + Min and the other be the Min + the Max
				temp = MIN(userValue(1), userValue(2))
				userValue(1) = temp + MIN(cardValue(1), cardValue(2))
				temp2 = MAX(userValue(1), userValue(2))
				userValue(2) = temp2 + MIN(cardValue(1), cardValue(2))
			ELSE IF (SIZE(cardValue) == 2) THEN		!If we draw an ace for the first card (userValue not yet been allocated)
				ALLOCATE(userValue(2))
				userValue(1) = cardValue(1)
				userValue(2) = cardValue(2)
			ELSE IF (SIZE(cardValue) == 1 .AND. ALLOCATED(userValue) .AND. SIZE(userValue) == 2) THEN	!if we have drawn an ace previously, and now drew something else
				userValue(1) = userValue(1) + cardValue(1)
				userValue(2) = userValue(2) + cardValue(1)
			ELSE IF (SIZE(cardValue) == 1 .AND. ALLOCATED(userValue)) THEN	!if we have drawn cards before and none (including this one) are aces
				userValue(1) = userValue(1) + cardValue(1)
			ELSE		!if this is our first card we are looking at and it's not an ace
				ALLOCATE(userValue(1))
				userValue(1) = cardValue(1)
			END IF
			
			DEALLOCATE(cardValue)
		END DO
		
		!Finds value of dealer's current hand
		!Same code as above, just for finding the dealer's value.
		!Probably should have done these in sepearate procedures cause the big enchilada led to problems.
		
		DO i = 1, dealerIndex
			CALL findValue(dealerHand(i), cardValue)
			
			IF (SIZE(cardValue) == 2 .AND. ALLOCATED(dealerValue) .AND. SIZE(dealerValue) == 1) THEN	!If we draw an ace for first time (and dealerValue already allocated to 1)
				temp = dealerValue(1)
				DEALLOCATE(dealerValue)
				ALLOCATE(dealerValue(2))
				dealerValue(1) = temp + cardValue(1)
				dealerValue(2) = temp + cardValue(2)
			ELSE IF (SIZE(cardValue) == 2 .AND. ALLOCATED(dealerValue)) THEN	!If we draw an ace for the second time
				temp = MIN(dealerValue(1), dealerValue(2))
				dealerValue(1) = temp + MIN(cardValue(1), cardValue(2))
				temp2 = MAX(dealerValue(1), dealerValue(2))
				dealerValue(2) = temp2 + MIN(cardValue(1), cardValue(2))
			ELSE IF (SIZE(cardValue) == 2) THEN		!If we draw an ace for the first card (dealerValue not yet been allocated)
				ALLOCATE(dealerValue(2))
				dealerValue(1) = cardValue(1)
				dealerValue(2) = cardValue(2)
			ELSE IF (SIZE(cardValue) == 1 .AND. ALLOCATED(dealerValue) .AND. SIZE(dealerValue) == 2) THEN	!if we have drawn an ace, and now drew something else
				dealerValue(1) = dealerValue(1) + cardValue(1)
				dealerValue(2) = dealerValue(2) + cardValue(1)
			ELSE IF (SIZE(cardValue) == 1 .AND. ALLOCATED(dealerValue)) THEN	!if we have drawn cards before and none (including this one) are aces
				dealerValue(1) = dealerValue(1) + cardValue(1)
			ELSE	!this is our first card we are looking at and it's not an ace
				ALLOCATE(dealerValue(1))
				dealerValue(1) = cardValue(1)
			END IF
			
			DEALLOCATE(cardValue)
		END DO
				
			
		
	END SUBROUTINE determineValues

	SUBROUTINE analyzeValues(userValue, dealerValue, userBJ, dealerBJ, userBust, dealerBust)
!
!	Uses the values found from determineValues and figures out if either player hsa a bust or Blackjack.
!
! VARIABLES USED:
! 	NAME: 					TYPE:     								COMMENT:
!	userValue				INTEGER, DIMENSION(:), ALLOCATABLE		Value of user's card hand calculated in determineValues
!	dealerValue				INTEGER, DIMENSION(:), ALLOCATABLE,		^ but for the dealer
!	userBJ					LOGICAL									Boolean for if the user has a blackjack in their hand
!	dealerBJ				LOGICAL									^ but for the dealer
!	userBust				LOGICAL									Boolean for if the user has busted
!	dealerBust				LOGICAL									^ but for the dealer
!	temp					INTEGER									Used to hold minimum value while reallocating when one ace value would result in bust
!----------------------------------------------------------------------------------------------------------------------------------------------------------
		INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: userValue, dealerValue
		LOGICAL, INTENT(OUT) :: userBJ, dealerBJ, userBust, dealerBust
		INTEGER :: temp
		
		!check for user statuses
		IF (ALLOCATED(userValue)) THEN
			IF (SIZE(userValue) == 2) THEN	!user has an ace, two potential values
				IF (userValue(1) > 21 .AND. userValue(2) > 21) THEN	!if user has ace and over 21 either way, bust
					userBust = .TRUE.
					DEALLOCATE(userValue)
				ELSE IF (userValue(1) > 21 .OR. userValue(2) > 21) THEN	!if user has ace and one is bust but the other way isnt, drop value with bust
					temp = MIN(userValue(1), userValue(2))
					DEALLOCATE(userValue)
					ALLOCATE(userValue(1))
					userValue(1) = temp
				ELSE IF (userValue(1) == 21 .OR. userValue(2) == 21) THEN	!if user has a blackjack with either value
					userBJ = .TRUE.
				END IF
			ELSE	!user has no aces, only one potential value
				IF (userValue(1) > 21) THEN
					userBust = .TRUE.
				ELSE IF (userValue(1) == 21) THEN
					userBJ = .TRUE.
				END IF
			END IF
		END IF
		
		!check for dealer statuses
		IF (ALLOCATED(dealerValue)) THEN
			IF (SIZE(dealerValue) == 2) THEN	!dealer has an ace, two potential values
				IF (dealerValue(1) > 21 .AND. dealerValue(2) > 21) THEN	!if dealer has ace and over 21 either way, bust
					dealerBust = .TRUE.
					DEALLOCATE(dealerValue)
				ELSE IF (dealerValue(1) > 21 .OR. dealerValue(2) > 21) THEN	!if dealer has ace and one is bust but the other way isnt, drop value with bust
					temp = MIN(dealerValue(1), dealerValue(2))
					DEALLOCATE(dealerValue)
					ALLOCATE(dealerValue(1))
					dealerValue(1) = temp
				ELSE IF (dealerValue(1) == 21 .OR. dealerValue(2) == 21) THEN	!if dealer has a blackjack with either value
					dealerBJ = .TRUE.
				END IF
			ELSE	!dealer has no aces, only one potential value
				IF (dealerValue(1) > 21) THEN
					dealerBust = .TRUE.
				ELSE IF (dealerValue(1) == 21) THEN
					dealerBJ = .TRUE.
				END IF
			END IF
		END IF
		
	
	END SUBROUTINE analyzeValues
	
	SUBROUTINE checkOutcome(dBJ, uBJ, uBust, dBust, outcome)
!
!	Assigns either a win, push, or loss outcome to the user based on logicals from analyzeValues.
!
! VARIABLES USED:
! 	NAME: 					TYPE:     							COMMENT:
!	dBJ						LOGICAL							Will be true if dealer has a blackjack
!	uBJ						LOGICAL							^ but for the user
!	uBust					LOGICAL							Will be true if the user has busted
!	dBust					LOGICAL							^ but for the dealer
!	outcome					CHARACTER						stores a character for any game outcome that may have been determined
!---------------------------------------------------------------------------
		LOGICAL, INTENT(IN) :: dBJ, uBJ, uBust, dBust
		CHARACTER, INTENT(OUT) :: outcome
		
		IF (uBJ) THEN	!user blackjack either win or push
			IF (dBJ) THEN	!push
				outcome = 'p'
			ELSE	!win
				outcome = 'w'
			END IF
		ELSE IF (dBJ) THEN	!if dealer blackjacks but user hasn't, loss
			outcome = 'l'
		END IF
		
		
		
		IF (uBust) THEN	!if user bust, loss
			outcome = 'l'
		ELSE IF(dBust) THEN	!if dealer busts but user hasn't, win
			outcome = 'w'
		END IF
	
	END SUBROUTINE checkOutcome
	
	SUBROUTINE consequences(outcome, uBJ, chips, bet)
!
!	Ends the game and dishes out rewards or takes away losses when an outcome has occured in a round.
!
! VARIABLES USED:
! 	NAME: 					TYPE:     							COMMENT:
!	outcome					CHARACTER							Holds character of whatever outcome was found in checkOutcome. 'w' = win, 'p' = push, 'l' = loss
!	uBJ						LOGICAL								Needs to be here because win by Blackjack pays 2:1 instead of 1:1
!	bet						INTEGER								Amount the user bet on this round
!	chips					INTEGER								Amount of chips user currently has
!---------------------------------------------------------------------------
		CHARACTER, INTENT(IN) :: outcome
		LOGICAL, INTENT(IN) :: uBJ
		INTEGER, INTENT(IN) :: bet
		INTEGER, INTENT(INOUT) :: chips
		
		IF (outcome /= 'n') THEN
			SELECT CASE (outcome)
				CASE ('w')
					IF (uBJ) THEN	!blackjack pays 2:1
						chips = chips + (bet * 2)
						WRITE(*,'(A, I0, A, I0, A)') "Wooo that's a blackjack! You just earned ", bet * 2, " chips! Now you've got ", chips," chips"
					ELSE
						chips = chips + bet
						WRITE(*,'(A, I0, A, I0, A)') "Congrats on the win. That's ", bet, " for a total of ", chips, "chips now."
					END IF
				CASE ('l')
					chips = chips - bet
					WRITE(*,'(A, I0, A, I0, A)') "What a shame, you just lost ", bet, " chips. Now you've only got ", chips, " left."
				CASE('p')
					WRITE(*,'(A, I0, A)') "No gain, no loss. You still got ", chips, " chips."
			END SELECT
		END IF
	END SUBROUTINE consequences
	
	!Subroutine for each round of the game
	SUBROUTINE playRound(bet, deck, deckPlace, chips)
!
!	I know I called determineValues the big enchilada earlier, but this subroutine really is the big enchilada.
!	Has majority of the game's functionality and almost trapped me in an infinite loop irl making it. Wasted multiple
!	hours writing code in circles on this one before I decided to completely gut it and start from the beginning.
!
! VARIABLES USED:
! 	NAME: 					TYPE:     							COMMENT:
!	deck				CHARACTER(LEN=2), DIMENSION(52)			Since this contains the bulk of functionality, most variables here
!	bet					INTEGER									are used and have already been explained or commented on in the previous subroutines.
!	deckPlace			INTEGER									If you're really curious about one you see here just CTRL-F or something Idk
!	chips				INTEGER									
!	userValue			INTEGER, DIMENSION(:), ALLOCATABLE		I will explain that I made these allocatable arrays because I needed to store the
!	dealerValue			INTEGER, DIMENSION(:), ALLOCATABLE		numerical value of cards, but aces are a little tricky since they could be 1 or 11,
!	userIndex			INTEGER									so I decided that it was at least a decent approach to have all cards/hands be arrays, and
!	dealerIndex			INTEGER									any hand containing an ace would be an array of size 2 so that it could hold both possible values
!	i					INTEGER									that an ace allows for. There may have been a smarter way to pull this off, but it was the best I
!	temp				INTEGER									could come up with in my sleep deprived brain.
!	userHand			CHARACTER(LEN=2), DIMENSION(26)
!	dealerHand			CHARACTER(LEN=2), DIMENSION(26)			I also chose size 26 for the hands because even though it's impossible to have 26 cards without busting
!	userChoice			CHARACTER								busting in Blackjack, I wanted to have enough space in case something went really funky. Figured half 
!	dealerChoice		CHARACTER								of the deck wasn't a terrible number to land on. The userIndex and dealerIndex essentially keep
!	outcome				CHARACTER								track of the true size of each one's hand.
!	userBJ				LOGICAL
!	dealerBJ			LOGICAL
!	userBust			LOGICAL
!	dealerBust			LOGICAL
!	firstLoop			LOGICAL									Made this because there was stuff in loops that I wanted to have, but didn't want to happen on the first iteration.
!--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
		CHARACTER(LEN=2), DIMENSION(52), INTENT(INOUT) :: deck
		INTEGER, INTENT(IN) :: bet
		INTEGER, INTENT(INOUT) :: deckPlace, chips
		INTEGER, DIMENSION(:), ALLOCATABLE :: userValue, dealerValue
		INTEGER :: userIndex, dealerIndex, i, temp
		CHARACTER(LEN=2), DIMENSION(26) :: userHand, dealerHand
		CHARACTER :: userChoice = 'z', dealerChoice = 'z', outcome = 'n'
		LOGICAL :: userBJ = .FALSE., dealerBJ = .FALSE., userBust = .FALSE., dealerBust = .FALSE., firstLoop = .TRUE.
		
		!Initial deal
		CALL checkForEnd(deckPlace, deck)	!If we're at the end of the deck, reshuffle cards. Do this every time before a card is dealt.
		userHand(1) = deck(deckPlace)
		deckPlace = deckPlace + 1
		CALL checkForEnd(deckPlace, deck)
		WRITE(*,*) "Your Hand: ", userHand(1)
		CALL sleep(1)
		dealerHand(1) = deck(deckPlace)
		deckPlace = deckPlace + 1
		CALL checkForEnd(deckPlace, deck)
		WRITE(*,*) "Dealer's Hand: ", dealerHand(1)
		CALL sleep(1)
		userHand(2) = deck(deckPlace)
		deckPlace = deckPlace + 1
		CALL checkForEnd(deckPlace, deck)
		WRITE(*,*) "Your Hand: ", userHand(1), " ", userHand(2)
		CALL sleep(1)
		dealerHand(2) = deck(deckPlace)
		deckPlace = deckPlace + 1
		CALL checkForEnd(deckPlace, deck)
		WRITE(*,*) "Dealer's Hand: ", dealerHand(1), " ?"
		userIndex = 2	!user and player are both now at two cards
		dealerIndex = 2
		
		!re-initialize variables for consecutive rounds
		userBJ = .FALSE.
		dealerBJ = .FALSE.
		userBust = .FALSE.
		dealerBust = .FALSE.
		firstLoop = .TRUE.
		outcome = 'n'
		dealerChoice = 'z'
		userChoice = 'z'
		
		WRITE(*,*) "********************************************"
		
		
		CALL determineValues(userHand, dealerHand, userIndex, dealerIndex, userValue, dealerValue)	!Find values of both hands
		
		!Tell user value of their hand
		IF (ALLOCATED(userValue) .AND. SIZE(userValue) == 2) THEN	!user has two possible values
			WRITE(*,'(A, I0, A, I0)') "Your hand is either a ", userValue(1), " or a ", userValue(2)
		ELSE IF (ALLOCATED(userValue)) THEN
			WRITE(*,'(A, I0)') "Your hand is a ", userValue(1)
		END IF
		
		CALL analyzeValues(userValue, dealerValue, userBJ, dealerBJ, userBust, dealerBust)	!check for ending conditions
		IF (userBJ .OR. dealerBJ .OR. userBust .OR. dealerBust) THEN
			CALL checkOutcome(dealerBJ, userBJ, userBust, dealerBust, outcome)	!check what outcome is
			
			WRITE(*,'(A)', advance='no') "Dealer's Hand: "
			DO i = 1, dealerIndex
				CALL sleep(1)
				WRITE(*,'(A, A)', advance='no') dealerHand(dealerIndex), " "
			END DO
			WRITE(*,*)
			
			CALL consequences(outcome, userBJ, chips, bet)
			RETURN
			
		ELSE	!continue game if no outcomes occured
		
			DO WHILE (userChoice /= 's' .AND. userChoice /= 'S')
				IF (.NOT.(firstLoop)) THEN
					WRITE(*,*) "********************************************"
					!draw out dealer's cards
					CALL sleep(1)
					WRITE(*,'(A)', advance='no') "Dealer's Hand: "
					WRITE(*,'(A, A)', advance='no') dealerHand(1), " ?"
					WRITE(*,*)
					
					!user gains a card
					userIndex = userIndex + 1
					userHand(userIndex) = deck(deckPlace)
					deckPlace = deckPlace + 1
					CALL checkForEnd(deckPlace, deck)
					
					!draw out user's cards
					WRITE(*,'(A)', advance='no') "Your Hand: "
					DO i = 1, userIndex
						WRITE(*,'(A, A)', advance='no') userHand(i), " "
						CALL sleep(1)
					END DO
					WRITE(*,*)
					
					!Calculate new hand value
					CALL determineValues(userHand, dealerHand, userIndex, dealerIndex, userValue, dealerValue)	!Find values of both hands
					
					!Tell user value of their hand
					IF (ALLOCATED(userValue) .AND. SIZE(userValue) == 2) THEN	!user has two possible values
						WRITE(*,'(A, I0, A, I0)') "Your hand is either a ", userValue(1), " or a ", userValue(2)
					ELSE IF (ALLOCATED(userValue)) THEN
						WRITE(*,'(A, I0)') "Your hand is a ", userValue(1)
					END IF
					
					!Analyze hand and check for ending conditions
					CALL analyzeValues(userValue, dealerValue, userBJ, dealerBJ, userBust, dealerBust)	!check for ending conditions
					IF (userBJ .OR. dealerBJ .OR. userBust .OR. dealerBust) THEN
						CALL checkOutcome(dealerBJ, userBJ, userBust, dealerBust, outcome)	!check what outcome is
						CALL sleep(1)
						CALL consequences(outcome, userBJ, chips, bet)
						RETURN
					END IF
						

					WRITE(*,'(A, A)') "The dealer's still showing a(n) ", dealerHand(1)
					
					WRITE(*,*) "So... hit or stand? (h/s)"
					READ(*,*) userChoice
					
				!user jumps to here if it's the first loop so they don't draw a card without choosing to hit or stand
				ELSE
					WRITE(*,*) "So... hit or stand? (h/s)"
					READ(*,*) userChoice
					firstLoop = .FALSE.
					
					IF (userChoice == 'h' .OR. userChoice == 'H') THEN	!cycle to top of loop if they choose to hit
						CYCLE
					END IF
				END IF
			END DO

				
					
			!Loop ended so user hasn't won/lost/pushed and is standing
			WRITE(*,*) "********************************************"
			
			!draw out user's cards
			WRITE(*,'(A)', advance='no') "Your Hand: "
			DO i = 1, userIndex
				CALL sleep(1)
				WRITE(*,'(A, A)', advance='no') userHand(i), " "
			END DO
			WRITE(*,*)
	
			IF (ALLOCATED(userValue) .AND. SIZE(userValue) == 2) THEN
				IF (MAX(userValue(1), userValue(2)) > 21) THEN	!one of user's values is a bust. get rid of it and give user the min value
					temp = MIN(userValue(1), userValue(2))
					DEALLOCATE(userValue)
					ALLOCATE(userValue(1))
					userValue(1) = temp
					
					WRITE(*,'(A,I0)') "Alright, you're standing on a(n) ", userValue(1)
				ELSE	!user has two possible values and neither is bust, give highest value
					temp = MAX(userValue(1), userValue(2))
					DEALLOCATE(userValue)
					ALLOCATE(userValue(1))
					userValue(1) = temp
					
					WRITE(*,'(A,I0)') "Alright, you're standing on a(n) ", userValue(1)
				END IF
			ELSE IF (ALLOCATED(userValue)) THEN		!user only has one value and that's where they're standing
				WRITE(*,'(A,I0)') "Alright, you're standing on a(n) ", userValue(1)
			END IF
					
					
			!Now dealer hits until >17 or bust
			firstLoop = .TRUE.
			DO WHILE (dealerChoice /= 's')
				WRITE(*,*) "********************************************"
			
				IF(.NOT.(firstLoop)) THEN
					!dealer draws a card
					dealerIndex = dealerIndex + 1
					dealerHand(dealerIndex) = deck(deckPlace)
					deckPlace = deckPlace + 1
					CALL checkForEnd(deckPlace, deck)
				ELSE
					firstLoop = .FALSE.
				END IF
				
				!draw out dealer's cards
				CALL sleep(1)
				WRITE(*,'(A)', advance='no') "Dealer's Hand: "
				DO i = 1, dealerIndex
					CALL sleep(1)
					WRITE(*,'(A,A)', advance='no') dealerHand(i), " "
				END DO
				WRITE(*,*)
					
				!Calculate new hand value
				CALL determineValues(userHand, dealerHand, userIndex, dealerIndex, userValue, dealerValue)	!Find values of both hands
				
				IF (ALLOCATED(dealerValue) .AND. SIZE(dealerValue) == 2) THEN	!if dealer's holding an ace
					IF (MAX(dealerValue(1), dealerValue(2)) <= 21) THEN		!if neither possible values are a bust, take highest one
						WRITE(*,'(A,I0)') "Dealer has ", MAX(dealerValue(1), dealerValue(2))
					ELSE	!if one is a bust, take the lower one
						WRITE(*,'(A,I0)') "Dealer has ", MIN(dealerValue(1), dealerValue(2))
					END IF
				ELSE
					WRITE(*,'(A,I0)') "Dealer has ", dealerValue(1)
				END IF
				
				!Analyze hand and check for ending conditions
				CALL analyzeValues(userValue, dealerValue, userBJ, dealerBJ, userBust, dealerBust)	!check for ending conditions
				IF (userBJ .OR. dealerBJ .OR. userBust .OR. dealerBust) THEN
					CALL checkOutcome(dealerBJ, userBJ, userBust, dealerBust, outcome)	!check what outcome is
					CALL consequences(outcome, userBJ, chips, bet)
					RETURN
				END IF
				
				
						
				IF (ALLOCATED(dealerValue) .AND. SIZE(dealerValue) == 2) THEN	!dealer has an ace (two values)
					WRITE(*,'(A,I0)') "Dealer is holding a ", MAX(dealerValue(1), dealerValue(2))
					IF (dealerValue(1) >= 17 .OR. dealerValue(2) >= 17)	THEN !At least one value >=17 and not a bust
						dealerChoice = 's'	!dealer will stand
						
						!get rid of the lower value to make this less complicated
						temp = MAX(dealerValue(1), dealerValue(2))
						DEALLOCATE(dealerValue)
						ALLOCATE(dealerValue(1))
						dealerValue(1) = temp
						
						CYCLE
					ELSE
						CYCLE
					END IF
					
				ELSE	!dealer has only one possible value (no ace)
					IF(dealerValue(1) >= 17) THEN	!dealer has 17 or above and stands, exiting the loop
						dealerChoice = 's'
						CYCLE
					!otherwise dealer has less than a 17 and hits, restarting the loop
					END IF
				END IF
			END DO
			
			CALL sleep(1)
			!Now it comes down to comparing card values
			WRITE(*,*) "********************************************"
			
			!draw out dealer's cards
			WRITE(*,'(A)', advance='no') "Dealer's Hand: "
			DO i = 1, dealerIndex
				CALL sleep(1)
				WRITE(*,'(A,A)', advance='no') dealerHand(i), " "
			END DO
			WRITE(*,*)

			!draw out user's cards one last time
			WRITE(*,'(A)', advance='no') "Your Hand: "
			DO i = 1, userIndex
				CALL sleep(1)
				WRITE(*,'(A, A)', advance='no') userHand(i), " "
			END DO
			WRITE(*,*)
			
			
			
			
			!Re-do allocation for values (calls to determineValues can override it)
			IF (ALLOCATED(userValue) .AND. SIZE(userValue) == 2) THEN
				IF (MAX(userValue(1), userValue(2)) > 21) THEN	!one of user's values is a bust. get rid of it and give user the min value
					temp = MIN(userValue(1), userValue(2))
					DEALLOCATE(userValue)
					ALLOCATE(userValue(1))
					userValue(1) = temp
				ELSE	!user has two possible values and neither is bust, give highest value
					temp = MAX(userValue(1), userValue(2))
					DEALLOCATE(userValue)
					ALLOCATE(userValue(1))
					userValue(1) = temp
				END IF
			ELSE IF (ALLOCATED(userValue)) THEN	!idk why but without this line the code breaks
			END IF
			
			IF (ALLOCATED(dealerValue) .AND. SIZE(dealerValue) == 2) THEN	!if dealer's holding an ace
				IF (MAX(dealerValue(1), dealerValue(2)) <= 21) THEN		!if neither possible values are a bust, take highest one
					temp = MAX(dealerValue(1), dealerValue(2))
					DEALLOCATE(dealerValue)
					ALLOCATE(dealerValue(1))
					dealerValue(1) = temp
				ELSE	!if one is a bust, take the lower one
					temp = MIN(dealerValue(1), dealerValue(2))
					DEALLOCATE(dealerValue)
					ALLOCATE(dealerValue(1))
					dealerValue(1) = temp
				END IF
			ELSE IF (ALLOCATED(dealerValue)) THEN	!it's literally pointless but I have to keep it for some reason
			END IF
			
			
			CALL sleep(1)
			!declare final tally
			WRITE(*,'(A, I0, A, I0)') "You stand at a ", userValue(1), " against the dealer's ", dealerValue(1)
			CALL sleep(1)
			
			!compare numbers and dish out winnings or take losses
			IF (userValue(1) > dealerValue(1)) THEN
				outcome = 'w'
			ELSE IF (userValue(1) == dealerValue(1)) THEN
				outcome = 'p'
			ELSE
				outcome = 'l'
			END IF
			
			CALL consequences(outcome, userBJ, chips, bet)
		
		END IF
							
	END SUBROUTINE playRound
	
	
	
END MODULE cards

PROGRAM Blackjack
!
! The beautiful driver program for the absolute spaghetti mess that lies 2 lines above it.
! This part basically just does file I/O for the wallet and keeps the user playing rounds until they get bored or go broke.
!
! VARIABLES USED:
! 	NAME: 					TYPE:     							COMMENT:
!	deck				CHARACTER(LEN=2), DIMENSION(52)			Same deck variable from above procedures
!	playedBefore		CHARACTER								Determines if the user already has a wallet.dat to use or not
!	playAgain			CHARACTER								'y' means user wants to play again, 'n' means quit program
!	msg					CHARACTER(LEN=80)						Stores error message in case file I/O goes wrong
!	i					INTEGER									think this might just be a relic left from an older point in this program's lifespan
!	chips				INTEGER									Amount of chips player has in wallet
!	istat				INTEGER									Error indicator for file I/O
!	bet					INTEGER									Amount of moolah user is willing to put down in one round
!	deckPlace			INTEGER									Again, essentially a pointer. Holds the index of the next card to be dealt. Resets and reshuffles at 52.
!---------------------------------------------------------------------------
USE cards
IMPLICIT NONE !Must explicitly declare all variables

!Declare the variables and Initialize
CHARACTER(LEN=2), DIMENSION(52) :: deck
CHARACTER :: playedBefore = 'n', playAgain = 'y'
CHARACTER(LEN=80) :: msg
INTEGER :: i, chips, istat, bet, deckPlace = 1
	
	
	CALL shuffle(deck)	!shuffle the deck
	
	!check if user already has a wallet
	WRITE(*,*) "Welcome to Blackjack! Have you played this game with us before? (y/n)"
	READ(*,*) playedBefore
	IF (playedBefore == 'y' .OR. playedBefore == 'Y') THEN
		OPEN(UNIT=9, FILE='wallet.dat', STATUS='OLD', ACTION='READ', IOSTAT=istat, IOMSG=msg)
		IF (istat == 0) THEN
			READ(9,*) chips
			WRITE(*,'(A, I0, A)') "Alrighty, looks like you've got a heap of ", chips, " chips from the last time you were here."
		ELSE
			WRITE(*,900) TRIM(msg)
			900 FORMAT ("File open failed. Status = ", A)
			WRITE(*,*) "Hmmm I'm not seeing your wallet anywhere. I'll give ya 250 chips to start"
			chips = 100
		END IF
		CLOSE(UNIT=9, STATUS='KEEP')
	ELSE
		WRITE(*,*) "No worries, here's a stash of 100 chips to start you off"
		chips = 100
	END IF
	
	
	DO WHILE ((playAgain == 'y' .OR. playAgain == 'Y'))
		!Get user's bet
		WRITE(*,*) "********************************************"
		WRITE(*,*) "Let's get started. How much you bettin'?"
		READ(*,*) bet
		DO WHILE (bet > chips .OR. bet <= 0)
			WRITE(*,'(A, I0)') "Nice try, but you can't bet more than you've got. Gimme a non-negative number under ", chips
			READ(*,*) bet
		END DO
		WRITE(*,*) "********************************************"
		
		CALL playRound(bet, deck, deckPlace, chips)
		
		
		WRITE(*,*) "********************************************"
		WRITE(*,*) "Good round, you wanna go again? (y/n)"
		
		!Write to wallet to save # of chips
		OPEN(UNIT=9, FILE='wallet.dat', STATUS='UNKNOWN', ACTION='WRITE', IOSTAT=istat, IOMSG=msg)
		IF (istat == 0) THEN
			WRITE(9,*) chips
			WRITE(*,'(A, I0, A)') "I went ahead and saved your chips saved in your wallet by the way."
		ELSE
			WRITE(*,800) TRIM(msg)
			800 FORMAT ("File open failed. Status = ", A)
		END IF
		CLOSE(UNIT=9, STATUS='KEEP')
		
		
		READ(*,*) playAgain
		
		IF ((playAgain == 'y' .OR. playAgain == 'Y') .AND. chips <= 0) THEN
			WRITE(*,*) "Slow down there, eager beaver. Looks like we bled you dry. Well, better luck next time."
			EXIT
		END IF
	END DO
	
	WRITE(*,*) "Thanks for stopping by the casino. We're always obliged to take your dough"
	
	
END PROGRAM Blackjack