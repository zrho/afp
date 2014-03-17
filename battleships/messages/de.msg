GameName: Schiffe versenken
Credits: Präsentiert von Projektgruppe AFP
GameDescription: 
IEWarning: Anscheinend betrachtest du unsere Seite im IE. Wenn du dabei auf irgendwelche Probleme stößt, benutze bitte einen Browser wie z.B. Chrome oder Firefox.
NoJavaScriptWarning: Anscheinend hast du JavaScript deaktiviert. Unsere Seite benötigt dies aber, um richtig zu funktionieren. Bitte aktiviere JavaScript!
LinkHome: Zurück zur Startseite
StartPlacing: Schiffe platzieren
FireShot: Schuss abfeuern!
Won: Du hast gewonnen!
WonByTimeout remShipsHuman@Int remShipsComputer@Int: Du hast durch Timeout gewonnen! Du hast #{show remShipsHuman} ungesunkene Schiffe, der Computer nur #{show remShipsComputer}.
Lost: Du hast verloren.
LostByTimeout remShipsHuman@Int remShipsComputer@Int: Du hast durch Timeout verloren! Du hast nur #{show remShipsHuman} ungesunkene Schiffe, der Computer #{show remShipsComputer}.
Drawn remShips@Int: Unentschieden! Die maximale Zuganzahl wurde erreicht und ihr habt beide #{show remShips} ungesunkene Schiffe.
PlaceShips: Platziere deine Schiffe
AvailableShips: Verfügbare Flotte
NoPossiblePlacement: Es konnte keine vollständige Flottenbelegung erzeugt werden.

InputAgainWhenHit: bei Treffer erneut schießen
InputMove: Schiffe verschiebbar
InputNoviceMode: Zusätzlich als Hilfe unmögliche Positionen anzeigen
InputDevMode: Entwicklermodus
InputDifficulty: Schwierigkeitsstufe
InputDifficultyEasy: Einfach
InputDifficultyMedium: Mittelschwer
InputDifficultyHard: Schwer
AdjustRules: Die Regeln anpassen
Rules: Regeln

ShipLength: Länge
ShipsAvailable: Verfügbar
ShipsTotal: Total
ResetFleet: Flotte zurücksetzen
PlaceRandom: Zufällig
StartPlaying: Los!
SkipMove: Ziehen überspringen
MoveShip: Bewege eins deiner Schiffe
YourFleet: Deine Flotte
OpponentsFleet: Gegnerische Flotte
OpponentRemainingShips: Folgende Schiffe musst du noch finden
ShipsUnsunk: Noch nicht gesunken
Hints: Hinweise
RemainingTurns n@Int: Nur noch #{show n} verbleibende Spielzüge!

Legend: Legende
LegendShipWithArrow: Schiff durch Klick bewegen
LegendShipMovable: Bewegliches Schiff
LegendShipImmovable: Unbewegliches Schiff
LegendShipHit: Treffer
LegendShipSunk: Treffer, Versenkt
LegendFogOfWar: Nicht kartografierter Bereich
LegendWater: Wasser
LegendLastShot: Feindliche Schüsse des letzten Zugs

PauseButtonLabel: Anhalten
UnpauseButtonLabel: Weiter
RestartButtonLabel: Von vorne
WatchReplay: Replay anschauen

ConfirmLeave: Wirklich zurück zur Startseite?
ConfirmReplay: Wirklich das Replay anschauen? Das beendet das aktuelle Spiel!
About: Über das Spiel
HowToPlay: So wird gespielt
HowToPlaceShipsHeading: Schiffe platzieren
HowToPlaceShipsFull: Zu Beginn des Spiels platzierst du deine Schiffe. Dafür drückst du die linke Maustaste im ersten der Felder, die dein Schiff belegen soll, und lässt sie im letzten zum Schiff gehörenden Feld wieder los. Um ein Schiff wieder zu entfernen, klick es einfach an; dann kannst du es an einer anderen Stelle platzieren. Die Felder um platzierte Schiffe herum werden ausgegraut, da Schiffe sich nicht berühren dürfen. Es ist auch möglich, eine zufällige Platzierung generieren zu lassen.
HowToPlayHeading: Schüsse feuern und Schiffe bewegen
HowToPlayFull: Nach dem Platzieren der Schiffe sind abwechselnd du und der Computer an der Reihe. Ein Spielzug besteht aus dem Feuern (wenn entsprechend eingestellt, darfst du so lange feuern, wie du Schiffe des Gegners triffst) und -optional- dem Bewegen eines unversehrten Schiffes um ein Feld. Zum Feuern klicke einfach auf das Feld, auf das du schießen möchtest. Die unversehrten Schiffe werden besonders hervorgehoben und die, die sich bewegen lassen (wenn also kein Hindernis den Weg blockiert) haben Pfeile an ihren Enden. Klicke auf einen Pfeil, um das zugehörige Schiff zu bewegen. Wichtig ist dabei: Gesunkene Schiffe verschwinden! Sobald ein Schiff also komplett versenkt wurde, kann man andere Schiffe darüber hinweg bewegen. 
HowToWinHeading: Spielende
HowToWinFull maxTurns@Int countdownTurns@Int: Das Spiel endet, sobald ein Spieler das letzte Schiff seines Gegners versenkt. Er gewinnt das Spiel dadurch. Um langweilige, niemals endende Spiele zu vermeiden, endet das Spiel jedoch auch nach maximal #{show maxTurns} Zügen. Um dich auf das bevorstehende Ende durch Timeout aufmerksam zu machen (damit du eventuell deine Strategie anpassen kannst), siehst du während der letzten #{show countdownTurns} Züge einen Countdown. Nach Timeout gewinnt der Spieler, der am meisten Schiffe versenkt hat.
ReallyFair: Spielt der Computer fair?
ReallyFairFull sourceURL@String: Ja, tut er - Er platziert und bewegt seine Schiffe nach den selben Regeln wie du und erhält auch keine zusätzlichen Informationen über deine Schiffe. Bei unserem Spiel gibt es einen 'Mittelsmann', der das Spiel leitet. Genau wie du sucht sich der Computer in seinem Zug aus, auf welches Feld er schießen möchte und teilt dies dem Mittelsmann mit. Die einzige Information, die der dem Computer zurückgibt, ist die, ob der Schuss ein Schiff getroffen bzw. versenkt hat oder nicht. Ebenso kann der Computer nicht über seine eigenen Schiffe 'lügen', denn auch deine Schüsse werden an den Mittelsmann weitergeleitet, der genau weiß, an welchen Stellen die Schiffe des Computers momentan liegen. Am Ende des Spiels siehst du eine animierte Zusammenfassung des Spielablaufs, um nachzuvollziehen, wann welches Schiff wo stand und wann wohin geschossen wurde. Wenn du besonders paranoid bist, kannst du dir auch den Quellcode anschauen (#{sourceURL}), um sicher zu gehen, dass der Computer nicht betrügt.
