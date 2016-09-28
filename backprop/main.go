package main

import "log"
import "flag"
import "ai/neural"

var debug = false

type Element struct {
	Health float64
	Knife  float64
	Gun    float64
	Enemy  float64
	Out    []float64
}

var samples = []Element{
	{2.0, 0.0, 0.0, 0.0, []float64{0.0, 0.0, 1.0, 0.0}},
	{2.0, 0.0, 0.0, 1.0, []float64{0.0, 0.0, 1.0, 0.0}},
	{2.0, 0.0, 1.0, 1.0, []float64{1.0, 0.0, 0.0, 0.0}},
	{2.0, 0.0, 1.0, 2.0, []float64{1.0, 0.0, 0.0, 0.0}},
	{2.0, 1.0, 0.0, 2.0, []float64{0.0, 0.0, 0.0, 1.0}},
	{2.0, 1.0, 0.0, 1.0, []float64{1.0, 0.0, 0.0, 0.0}},

	{1.0, 0.0, 0.0, 0.0, []float64{0.0, 0.0, 1.0, 0.0}},
	{1.0, 0.0, 0.0, 1.0, []float64{0.0, 0.0, 0.0, 1.0}},
	{1.0, 0.0, 1.0, 1.0, []float64{1.0, 0.0, 0.0, 0.0}},
	{1.0, 0.0, 1.0, 2.0, []float64{0.0, 0.0, 0.0, 1.0}},
	{1.0, 1.0, 0.0, 2.0, []float64{0.0, 0.0, 0.0, 1.0}},
	{1.0, 1.0, 0.0, 1.0, []float64{0.0, 0.0, 0.0, 1.0}},

	{0.0, 0.0, 0.0, 0.0, []float64{0.0, 0.0, 1.0, 0.0}},
	{0.0, 0.0, 0.0, 1.0, []float64{0.0, 0.0, 0.0, 1.0}},
	{0.0, 0.0, 1.0, 1.0, []float64{0.0, 0.0, 0.0, 1.0}},
	{0.0, 0.0, 1.0, 2.0, []float64{0.0, 1.0, 0.0, 0.0}},
	{0.0, 1.0, 0.0, 2.0, []float64{0.0, 1.0, 0.0, 0.0}},
	{0.0, 1.0, 0.0, 1.0, []float64{0.0, 0.0, 0.0, 1.0}},
}

var actions = []string{"Attack", "Run", "Wander", "Hide"}

func main() {
	iterations := flag.Int("n", 100000, "number of iterations")
	learnRate := flag.Float64("l", 0.2, "learn rate")
	hiddenNum := flag.Int("hidden", 3, "number of hidden neurons")
	flag.BoolVar(&debug, "v", debug, "enable debug printing")
	flag.Parse()

	p := neural.NewPerceptron(4, *hiddenNum, len(actions), *learnRate)
	teach(p, *iterations)
	testCorrectness(p)
	test(p)
}

func teach(p *neural.Perceptron, iterations int) {
	for {
		for _, sample := range samples {
			p.SetInputs([]float64{
				sample.Health,
				sample.Knife,
				sample.Gun,
				sample.Enemy,
			})
			p.SetTargets(sample.Out)
			p.FeedForward()
			if debug {
				log.Println("error = ", p.ErrValue())
			}
			iterations--
			if iterations == 0 {
				return
			}
			p.BackPropagate()
		}
	}

}

func testCorrectness(p *neural.Perceptron) {
	correct := 0
	for _, sample := range samples {
		inputs := []float64{
			sample.Health,
			sample.Knife,
			sample.Gun,
			sample.Enemy,
		}
		p.SetInputs(inputs)
		p.SetTargets(sample.Out)
		p.FeedForward()
		if action(p.Outputs()) != action(sample.Out) {
			log.Printf("Fail %+v : %v (%+v)", inputs, action(p.Outputs()), action(inputs))
		} else {
			correct++
		}
	}
	log.Printf("Network is %.2f%% correct", float64(correct)/float64(len(samples))*100.0)
}

func test(p *neural.Perceptron) {
	tests := [][]float64{
		{2, 1, 1, 1},
		{1, 1, 1, 2},
		{0, 0, 0, 0},
		{0, 1, 1, 1},
		{2, 0, 1, 3},
		{2, 1, 0, 3},
		{0, 1, 0, 3},
	}
	for _, test := range tests {
		p.SetInputs(test)
		p.FeedForward()
		log.Printf("%v action %s\n", test, action(p.Outputs()))
	}
}

func action(vector []float64) string {
	var max float64 = -1
	maxIndex := -1
	for i, v := range vector {
		if v > max {
			max = v
			maxIndex = i
		}
	}
	return actions[maxIndex]
}
