module Card.Main where
import Card.Data

card1 :: Card
card1 = Card {cid = 1, opened = False, details = Details}
card2 :: Card
card2 = Card {cid = 2, opened = True, details = Details}
card3 :: Card
card3 = Card {cid = 3, opened = False, details = Details}

handleOpened :: Card -> Card
handleOpened Card{cid = cid, opened = opened, details = details} =
    Card {cid = cid, opened = not opened, details = details}

deck1 :: [Card]
deck1 = [card1, card2, card3]