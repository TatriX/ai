package neural

import (
	"math"
	"math/rand"
)

type Perceptron struct {
	learnRate float64
	inputs    []*neuron
	hidden    []*neuron
	outputs   []*neuron
	targets   []float64
}

func NewPerceptron(numInputs, numHidden, numOutputs int, learnRate float64) *Perceptron {
	p := &Perceptron{
		learnRate: learnRate,
	}
	p.inputs = makeNeurons(numInputs)
	p.hidden = makeNeurons(numHidden)
	p.outputs = makeNeurons(numOutputs)
	p.targets = make([]float64, numOutputs)

	p.assignRandomWeights()
	return p
}

func makeNeurons(n int) []*neuron {
	neurons := make([]*neuron, n)
	for i := range neurons {
		neurons[i] = new(neuron)
	}
	return neurons
}

func (p *Perceptron) Outputs() []float64 {
	outputs := make([]float64, len(p.outputs))
	for i, output := range p.outputs {
		outputs[i] = output.value
	}
	return outputs
}

func (p *Perceptron) SetInputs(inputs []float64) {
	if len(inputs) != len(p.inputs) {
		panic("inputs len must match")
	}
	for i, value := range inputs {
		p.inputs[i].value = value
	}
}

func (p *Perceptron) SetTargets(targets []float64) {
	if len(targets) != len(p.targets) {
		panic("targets len must match")
	}
	p.targets = targets
}

func (p *Perceptron) assignRandomWeights() {
	assignRandomWeights(p.hidden, len(p.inputs))
	assignRandomWeights(p.outputs, len(p.hidden))
}

func assignRandomWeights(neurons []*neuron, n int) {
	for _, neuron := range neurons {
		for i := 0; i < n; i++ {
			neuron.weights = append(neuron.weights, randWeight())
		}
	}
}

type neuron struct {
	bias    float64
	value   float64
	weights []float64
	err     float64
}

func (p *Perceptron) FeedForward() {
	feedForward(p.inputs, p.hidden)
	feedForward(p.hidden, p.outputs)
}

func feedForward(from, to []*neuron) {
	for _, dst := range to {
		sum := dst.bias
		for i, src := range from {
			sum += src.value * dst.weights[i]
		}
		dst.value = sigmoid(sum)
	}
}

func (p *Perceptron) BackPropagate() {
	/* Calculate the output layer error (step 3 for output cell) */
	for i, output := range p.outputs {
		actual := output.value
		output.err = (p.targets[i] - actual) * sigmoidDerivative(actual)
	}

	/* Calculate the hidden layer error (step 3 for hidden cell) */
	for i, hidden := range p.hidden {
		hidden.err = 0
		for _, ouput := range p.outputs {
			hidden.err += ouput.err * ouput.weights[i]
		}
		hidden.err *= sigmoidDerivative(hidden.value)
	}
	updateWeights(p.hidden, p.outputs, p.learnRate)
	updateWeights(p.inputs, p.hidden, p.learnRate)
}

func updateWeights(inputs, outputs []*neuron, learnRate float64) {
	for _, output := range outputs {
		for i, input := range inputs {
			output.weights[i] += learnRate * output.err * input.value
		}
		output.bias += learnRate * output.err
	}
}

func (p *Perceptron) ErrValue() float64 {
	var err float64
	for i, neuroun := range p.outputs {
		diff := p.targets[i] - neuroun.value
		err += diff * diff
	}
	return 0.5 * err
}

func randWeight() float64 {
	return rand.Float64() - 0.5
}

func sigmoid(value float64) float64 {
	return 1 / (1 + math.Exp(-value))
}

func sigmoidDerivative(value float64) float64 {
	return value * (1 - value)
}
