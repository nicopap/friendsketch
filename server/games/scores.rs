#[derive(Debug)]
pub struct ScoreKeeper(u16, u16);

impl ScoreKeeper {
    pub fn new(player_count: u16) -> ScoreKeeper {
        ScoreKeeper(2_u16.pow(u32::from(player_count)), player_count)
    }

    pub fn next(&mut self) -> u16 {
        let prev = self.0;
        self.0 = if prev == 1 { 1 } else { prev / 2 };
        prev
    }
}
