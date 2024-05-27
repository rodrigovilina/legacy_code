fn main() {
    println!("Hello, world!");
}

const WIDTH: usize = 9;
const HEIGHT: usize = 9;

type Row = [CellState; WIDTH];

#[derive(Debug, Clone, Copy, PartialEq)]
enum CellState {
    Empty,
    Filled,
}

#[derive(PartialEq, Clone)]
struct Cell {
    state: CellState,
    row: usize,
    col: usize,
}


struct Board {
    rows: [Row; HEIGHT],
}

impl Board {
    fn new() -> Self {
        Self {
            rows: [[CellState::Empty; WIDTH]; HEIGHT],
        }
    }

    fn get_cell(&self, row: usize, col: usize) -> Option<Cell> {
        if row < HEIGHT && col < WIDTH {
            Some(Cell {
                state: self.rows[row][col],
                row,
                col,
            })
        } else {
            None
        }

    }

    fn cells(&self) -> Vec<Cell> {
        let mut cells = Vec::new();
        for row in 0..HEIGHT {
            for col in 0..WIDTH {
                cells.push(Cell {
                    state: self.rows[row][col],
                    row,
                    col,
                });
            }
        }
        cells
    }

    fn neighbors(&self, cell: &Cell) -> Vec<Cell> {
        let mut neighbors = Vec::new();
        for row in cell.row.saturating_sub(1)..=cell.row + 1 {
            for col in cell.col.saturating_sub(1)..=cell.col + 1 {
                if let Some(neighbor) = self.get_cell(row, col) {
                    if neighbor != cell.clone() {
                        neighbors.push(neighbor);
                    }
                }
            }
        }
        neighbors
    }

    fn neighbors_count(&self, cell: &Cell) -> usize {
        self.neighbors(cell).iter().filter(|c| c.state == CellState::Filled).count()
    }

    fn tick(self) -> Self {
        Self {
            rows: self.cells().iter().map(|cell| {
                let neighbors = self.neighbors_count(&cell);
                match cell.state {
                    CellState::Empty => {
                        if neighbors == 3 {
                            CellState::Filled
                        } else {
                            CellState::Empty
                        }
                    }
                    CellState::Filled => {
                        if neighbors == 2 || neighbors == 3 {
                            CellState::Filled
                        } else {
                            CellState::Empty
                        }
                    }
                }
            }).collect()
        }
    }
}

fn demo() -> [usize; 9] {
    let vector = vec![1, 2, 3, 4, 5, 6, 7, 8, 9];
    vector.try_into().unwrap()
}
