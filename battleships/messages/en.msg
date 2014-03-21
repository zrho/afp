GameName: Battleships
Credits: - Brought to you by Projektgruppe AFP
GameDescription: 
IEWarning: Looks like you're using IE. If you experience any problems using our site, please use a browser like Chrome or Firefox instead.
NoJavaScriptWarning: Looks like you don't have JavaScript enabled. To use our site properly, however, JavaScript is needed. Please enable it.
LinkHome: Go home
StartPlacing: Start placing your fleet
FireShot: Fire a shot!
Won: You Won!
WonByTimeout remShipsHuman@Int remShipsComputer@Int: You Won by timeout! You have #{show remShipsHuman} unsunk ships, the computer only has #{show remShipsComputer}.
Lost: You Lost.
LostByTimeout remShipsHuman@Int remShipsComputer@Int: You Lost by timeout! You have only #{show remShipsHuman} unsunk ships, the computer has #{show remShipsComputer}.
Drawn remShips@Int: It's a draw. The game timed out and both of you have #{show remShips} unsunk ships.
PlaceShips: Place your ships
AvailableShips: Available ships
NoPossiblePlacement: The fleet placement could not be completed.

InputAgainWhenHit: shoot again after making a hit
InputMove: allow ship movement
InputNoviceMode: additionally show hints about impossible positions
InputDevMode: enable developer mode
InputDifficulty: difficulty level
InputDifficultyEasy: easy
InputDifficultyMedium: medium
InputDifficultyHard: hard
AdjustRules: Adjust the rules
Rules: Rules

ShipLength: Length
ShipsAvailable: Available
ShipsTotal: Total
ResetFleet: Reset fleet
PlaceRandom: Random
StartPlaying: Ready!
SkipMove: Skip moving
MoveShip: Move one of your ships
YourFleet: Your Fleet
OpponentsFleet: Opponent's Fleet
OpponentRemainingShips: You still have to find these ships:
ShipsUnsunk: Not yet sunk
Hints: Hints
RemainingTurns n@Int: Only #{show n} turns remaining (after this one)!
LastTurn: Last turn!

Legend: Legend
LegendShipWithArrow: click to move the ship
LegendShipMovable: movable ship
LegendShipImmovable: immovable ship
LegendShipHit: hit
LegendShipSunk: sunk ship
LegendFogOfWar: undiscovered area
LegendWater: water
LegendLastShot: shot(s) of opponent's last turn
LegendWaterShots: Shots at water
LegendRightNow: Right now
LegendTurnsAgo n@Int: #{show n} turns ago
LegendMinTurnsAgo n@Int: At least #{show n} turns ago

PauseButtonLabel: Pause
UnpauseButtonLabel: Continue
RestartButtonLabel: Restart
WatchReplay: Watch replay
SaveGame: Save game
SaveGameExplanation: To save the current game state, bookmark the following link - this way you can always come back to the current game! Don't use this for cheating. :-)
CurrentGame: Bookmarkable link to the current game

ConfirmLeave: Do you really want to go home?
ConfirmReplay: Do you really want to see the replay now? This ends the current game!
About: About the game
HowToPlay: How to play
HowToPlaceShipsHeading: Placing your ships
HowToPlaceShipsFull: Before the actual game starts, you have to choose where to put your ships. To do so, you press the left mouse button in the first square of where you want to put your ship and release it in the last one. To delete one of your placed ships, just click it; you will then be able to put it somewhere else. The squares around your placed ships will become grey, as ships are not allowed to touch each other. It is also possible to let a random placement be generated (fully or in part).
HowToPlayHeading: Firing shots and moving ships
HowToPlayFull: After the initial placement phase, you and the computer take turns. Each turn consists of firing shots (if the according option is set, you get to fire again as long as you are hitting your opponent's ships) and -optionally- moving one of your undamaged ships by one square. You fire by just clicking the desired square in your opponent's field. Undamaged ships are highlighted; those which are movable (i.e. there is no obstacle in the way) have arrows at their ends. Just click one of the arrows to move a ship. Important: Once a ship is sunk, it disappears. This means that as soon as a ship is sunk, the other ships are able to move across it.
HowToWinHeading: End of the game
HowToWinFull maxTurns@Int countdownTurns@Int: The game ends when one player sinks the last one of the opponent's ships, thus winning the game. To avoid boring never-ending games, however, the game also ends after at most #{show maxTurns} turns. To make you aware of the impending timeout (you might want to alter your strategy accordingly), you will see a timeout during the last #{show countdownTurns} turns. After timeout, the player who has sunk the most ships has won.
ReallyFair: Does the computer play fair?
ReallyFairFull sourceURL@String aiURL@String: Yes, it does - When placing and moving its ships, it has to abide by the same rules as you. During the game it does not get any more information than you; there is a 'middleman' who controls the game. Just like you do, the computer decides on a square to fire at and tells the middleman its decision. The only information the computer gets in return is whether it hit / sunk a ship or just hit water. Besides, the computer cannot 'lie' about its ships, because the middleman knows where the computer's ships are at all times. After the game ends, you can watch an animated recap of the course of the game to comprehend what happened when. If you are extra paranoid, you may want to look at the source code (#{sourceURL}) to convince yourself that the computer does not cheat. To get a high-level idea of the computer's strategy, see #{aiURL}.
