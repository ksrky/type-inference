# type-inference

Type inference algorithms for functional programing languages.

|                 | Generate types and substitution | Propagate expected types inwards |
| --------------- | ------------------------------- | -------------------------------- |
| No side effects | Algorithm W                     | Algorithm M                      |
| Side effects    | Algorithm J                     | (Algorithm Z)                    |

## [Algorithm W](https://github.com/ksrky/type-inference/tree/master/src/algorithmW)

- Luis Damas, Robin Milner. (1982). [_Principal type-schemes for functional programs_](https://web.cs.wpi.edu/~cs4536/c12/milner-damas_principal_types.pdf).
- My blog commentary ([Japanese](https://zenn.dev/ksrk/articles/5e4a6858c43d6f))

Type `stack exec algorithmW` to test some expressions

## [Algorithm M](https://github.com/ksrky/type-inference/tree/master/src/algorithmM)

- Oukseh Lee, Kwangkeun Yi. (1998). [_Proofs about a Folklore Let-Polymorphic Type Inference Algorithm_](https://dl.acm.org/doi/10.1145/291891.291892).
- My blog commentary ([Japanese](https://zenn.dev/ksrk/articles/58c773163463d6))

Type `stack exec algorithmM` to test some expressions

## [Algorithm J](https://github.com/ksrky/type-inference/tree/master/src/algorithmJ)

- Robin Milner. (1978). [_A Theory of Type Polymorphism in Programming_](https://www.research.ed.ac.uk/en/publications/a-theory-of-type-polymorphism-in-programming).
- My blog commentary ([Japanese](https://zenn.dev/ksrk/articles/524444586ae23d))

Type `stack exec algorithmJ` to test some expressions

## [Algorithm Z](https://github.com/ksrky/type-inference/tree/master/src/algorithmZ)

Algorithm M with side effects. I named this algorithm.

Type `stack exec algorithmZ` to test some expressions

## [Type inference algorithm for arbitrary-rank types](https://github.com/ksrky/type-inference/tree/master/src/arbitrary-rank)

- Simon Peyton Jones, Dimitrios Vytiniotis, Stephanie Weirich, Mark Shields. (2005). [_Practical type inference for arbitrary-rank types_](https://www.microsoft.com/en-us/research/publication/practical-type-inference-for-arbitrary-rank-types/).

Type `stack exec arbitrary-rank` to test some expressions

## [System F](https://github.com/ksrky/type-inference/tree/master/src/SystemF)

Type inference of arbitrary rank types and System F translation

- Simon Peyton Jones, Dimitrios Vytiniotis, Stephanie Weirich, Mark Shields. (2005). [_Practical type inference for arbitrary-rank types_](https://www.microsoft.com/en-us/research/publication/practical-type-inference-for-arbitrary-rank-types/).
  Type inference algorithm for arbitrary-rank types

Type `stack exec systemF` to enter the interactive shell. If you input an semi-typed expression, then it is translated to System F-like term. For example:

```
$ stack exec systemF
>> \f -> let x = () in f x
/\a. \f : () -> a. let x : () = () in f x
>> \f -> f () :: (∀a. a -> a) -> ()
\f : ∀a. a -> a. (f [()]) ()
```
