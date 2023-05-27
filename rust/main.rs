#![allow(non_snake_case,non_camel_case_types,dead_code)]

/*
    Below is the function stub for deal. Add as many helper functions
    as you like, but the deal function should not be modified. Just
    fill it in.
    
    Test your code by running 'cargo test' from the war_rs directory.
*/

fn deal(shuf: &[u8; 52]) -> [u8; 52]
{
    shuf.into_iter().fold((Vec::new(), Vec::new()), |(mut player1, mut player2), c| {
        if player1.len() > player2.len() {
            player2.push(c);
        } else {
            player1.push(c);
        }
        (player1, player2)
    })
    
}


fn startgame(player1_tail: Vec<i32>, player2_tail: Vec<i32>, pile: Vec<i32>) -> Vec<i32> {
    match (player1_tail.first(), player2_tail.first()) {
        (Some(c1), Some(hd2)) => {
            let mut collection = vec![c1, hd2];
            collection.extend(pile);
            collection.sort_by(|a, b| b.cmp(a)); // sort in descending order
            if c1 > hd2 {
                startgame(player1_tail.into_iter().skip(1).chain(collection).collect(), player2_tail, Vec::new())
            } else if hd2 > c1 {
                startgame(player1_tail, player2_tail.into_iter().skip(1).chain(collection).collect(), Vec::new())
            } else if player1_tail.len() >= 1 && player2_tail.len() >= 1 {
                let (c1d, player1_tail_rest) = player1_tail.split_first().unwrap();
                let (c2d, player2_tail_rest) = player2_tail.split_first().unwrap();
                startgame(player1_tail_rest.to_vec(), player2_tail_rest.to_vec(), collection.into_iter().chain(vec![c1d, c2d]).collect())
            } else {
                startgame(player1_tail, player2_tail, collection)
            }
        },
        (None, Some(_)) => player2_tail.into_iter().chain(pile).rev().collect(),
        (Some(_), None) => player1_tail.into_iter().chain(pile).rev().collect(),
        _ => Vec::new()
    }
}

fn deal(shuf: Vec<i32>) -> Vec<i32> {
    let deck = shuf.into_iter().rev().map(|c| if c == 1 { 14 } else { c }).collect();
    let (player1, player2) = shuffler(deck);
    let winner = startgame(player1, player2, Vec::new());
    winner.into_iter().map(|c| if c == 14 { 1 } else { c }).collect()
}

#[cfg(test)]
#[path = "tests.rs"]
mod tests;

