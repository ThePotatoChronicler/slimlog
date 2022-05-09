# Short term TODOs

- [ ] Replace function returns from Result<Ins, String>
  with an Ok() type that can always be iterated.
  We want to be able to extend them (IntoIterator), but also
  not use Vec, to not have extranneous allocations.
- [ ] Replace tuple struct instructions with c-like structures,
  because I forgot those exist
- [ ] Check functions for allowing extra arguments
