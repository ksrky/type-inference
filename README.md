# type-inference

Type inference algorithms for functional programing languages.

|                 | Generate types and substitution | Propagate expected types inwards |
| --------------- | ------------------------------- | -------------------------------- |
| No side effects | Algorithm W                     | Algorithm M                      |
| Side effects    | Algorithm J                     | (Algorithm Z)                    |

## [Algorithm W](https://github.com/ksrky/type-inference/tree/master/src/algorithmW)

- Roger Hindley. (1969). [_The principle type-scheme of an object in combinatory logic_](https://www.ams.org/journals/tran/1969-146-00/S0002-9947-1969-0253905-6/home.html).
- My blog commentary ([Japanese](https://zenn.dev/ksrk/articles/5e4a6858c43d6f))

## [Algorithm M](https://github.com/ksrky/type-inference/tree/master/src/algorithmM)

- Oukseh Lee, Kwangkeun Yi. (1998). [_Proofs about a Folklore Let-Polymorphic Type Inference Algorithm_](https://dl.acm.org/doi/10.1145/291891.291892).
- My blog commentary ([Japanese](https://zenn.dev/ksrk/articles/58c773163463d6))

## [Algorithm J](https://github.com/ksrky/type-inference/tree/master/src/algorithmJ)

- Robin Milner. (1978). [_A Theory of Type Polymorphism in Programming_](https://www.research.ed.ac.uk/en/publications/a-theory-of-type-polymorphism-in-programming).
- My blog commentary ([Japanese](https://zenn.dev/ksrk/articles/524444586ae23d))

## [Algorithm Z](https://github.com/ksrky/type-inference/tree/master/src/algorithmZ)

- I named this algorithm.
- Algorithm M with side effects.

## [Type inference algorithm for arbitrary-rank types](https://github.com/ksrky/type-inference/tree/master/src/arbitrary-rank)

- Simon Peyton Jones, Dimitrios Vytiniotis, Stephanie Weirich, Mark Shields. (2005). [_Practical type inference for arbitrary-rank types_](https://www.microsoft.com/en-us/research/publication/practical-type-inference-for-arbitrary-rank-types/).
