enum Suit {
  Hearts,
  Diamonds,
  Clubs,
  Spades,
}

enum Rank {
  Two,
  Three,
  Four,
  Five,
  Six,
  Seven,
  Eight,
  Nine,
  Ten,
  Jack,
  Queen,
  King,
  Ace
}

enum BackColor {
  Red,
  Blue,
}

enum JokerColor {
  Red,
  Black,
}

enum Card {
  Normal {
    suit: Suit,
    rank: Rank,
    back_color: BackColor,
  },
  Joker {
    back_color: BackColor,
    joker_color: JokerColor,
  }
}

struct DrawDeck {
  cards: Vec<Card>,
}

struct PlayDeck {
  cards: Vec<Card>,
}

struct DiscardDeck {
  cards: Vec<Card>,
}

struct PlayerName(String);

struct CartasRepartidas {
  blind_hand: (Card, Card, Card),
  visible_hand: (Card, Card, Card),
  hand: (Card, Card, Card)
}

// (CartasRepartidas, PlayerName) -> Player

struct BlindHand(Maybe<Card>, Maybe<Card>, Maybe<Card>);
struct VisibleHand(Maybe<Card>, Maybe<Card>, Maybe<Card>);

struct Hand(Vec<Card>);

struct Player {
  name: PlayerName,
  blind_hand: BlindHand,
  visible_hand: VisibleHand,
  hand: Hand,
}

enum Direction {
  Clockwise,
  CounterClockwise,
}

struct Game {
  players: Vec<Player>,
  draw_deck: DrawDeck,
  play_deck: PlayDeck,
  discard_deck: DiscardDeck,
  current_player: PlayerName,
  direction: Direction,
}
