#include <iostream>
#include <vector>
#include <random>
#include <unistd.h>

using namespace std;

enum class Cell { dead, alive };

const bool debug = false;

class Point {
        friend ostream& operator<<(ostream& os, const Point& p);
public:
        int x, y;
        Point& operator+=(const Point& p) {
                x += p.x;
                y += p.y;
                return *this;
        }
};

ostream& operator<<(ostream& os, const Point& p) {
        return os << "(" << p.x << " " << p.y << ")";
}

class Pattern {
public:
        vector<Point> cells;
};

class Block: public Pattern  {
public:
        Block() {
                cells = {
                        {0, 0}, {1, 0},
                        {0, 1}, {1, 1},
                };
        };
};

class Glider: public Pattern {
public:
        Glider() {
                cells = {
                        {1, 2},
                        {2, 3},
                        {3, 1},
                        {3, 2},
                        {3, 3},
                };
        }
};


class Field {
        int w;
        int h;
        vector<vector<Cell>> cells;
public:
        friend ostream& operator<<(ostream& os, const Field& f);
        Field(int w, int h): w(w), h(h) {
                cells.reserve(h);
                for (auto y = 0; y < h; y++) {
                        cells[y].reserve(w);
                }
        }

        void add_pattern(Pattern pat, Point left_top) {
                for (auto p: pat.cells) {
                        p += left_top;
                        cells[p.y][p.x] = Cell::alive;
                }
        }

        void randomize() {
                random_device rd;
                mt19937 gen(rd());
                for (auto y = 0; y < h; y++) {
                        for (auto x = 0; x < w; x++) {
                                cells[y][x] = (gen() % 2 == 0) ? Cell::dead : Cell::alive;
                        }

                }
        }

        /*
          Any live cell with fewer than two live neighbours dies, as if caused by under-population.
          Any live cell with two or three live neighbours lives on to the next generation.
          Any live cell with more than three live neighbours dies, as if by over-population.
          Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
        */
        // f is a previous generation
        void next_generation(Field& f) {
                for (auto y = 0; y < h; y++) {
                        for (auto x = 0; x < w; x++) {
                                auto neighbours = f.neighbours(x, y);
                                if (debug) cout << neighbours;
                                switch (neighbours) {
                                case 2:
                                        cells[y][x] = f.cells[y][x];
                                        break;
                                case 3:
                                        cells[y][x] = Cell::alive;
                                        break;
                                default:
                                        cells[y][x] = Cell::dead;
                                        break;
                                }
                        }
                        if (debug) cout << endl;
                }
        }

        int neighbours(int x, int y) {
                const Point dir[] = {
                        {-1, -1}, {+0, -1}, {+1, -1},
                        {-1, +0}, /* * * */ {+1, +0},
                        {-1, +1}, {+0, +1}, {+1, +1},
                };
                int n = 0;
                for (auto i : dir) {
                        Point p = {x + i.x, y + i.y};
                        if (p.x >= w)
                                p.x %= w;
                        else if (p.x < 0)
                                p.x = w + p.x;

                        if (p.y >= h)
                                p.y %= h;
                        else if (p.y < 0)
                                p.y = h + p.y;

                        if (cells[p.y][p.x] == Cell::alive)
                                n++;
                }
                return n;
        }
};

ostream& operator<<(ostream& os, const Field& f) {
        for (auto y = 0; y < f.h; y++) {
                for (auto x = 0; x < f.w; x++) {
                        os << ((f.cells[y][x] == Cell::alive) ? "•" : "‧");
                }
                os << endl;
        }
        return os;
}

class Game {
        vector<Field> fields;
        int cur_field;
        friend ostream& operator<<(ostream& os, const Game& g);
        Field& current_field() {
                return fields[cur_field];
        }
public:
        Game(int w, int h): cur_field(0) {
                fields.push_back(Field(w, h));
                fields.push_back(Field(w, h));
        }

        void next_generation() {
                auto next_field = 1 - cur_field;
                fields[next_field].next_generation(fields[cur_field]);
                cur_field = next_field;
        }

        void add_pattern(Pattern pat, Point left_top) {
                current_field().add_pattern(pat, left_top);
        }

        void randomize() {
                current_field().randomize();
        }
};

ostream& operator<<(ostream& os, const Game& g) {
        return os << const_cast<Game&>(g).current_field();
}

void clearscr() {
        cout << "\x1B[2J\x1B[H";
}

int main()
{
        cout << "Wellcome to the  Game of life!" << endl;
        Game g(32, 16);
        char action;
        for(;;) {
                clearscr();
                cout << g;
                cout << "c: continue endless"
                     << ", n: next generation"
                     << ", b: add block"
                     << ", g: add glider"
                     << ", q: quit"
                     << endl;
                cout << "> ";
                cin >> action;
                switch (action) {
                case 'b':
                        g.add_pattern(Block(), Point{1, 1});
                        break;
                case 'g':
                        g.add_pattern(Glider(), Point{10, 10});
                        break;
                case 'c':
                        for(;;) {
                                clearscr();
                                g.next_generation();
                                cout << g;
                                sleep(1);
                        }
                case 'n':
                        g.next_generation();
                        break;
                case 'q':
                        return 0;
                }
        }
}