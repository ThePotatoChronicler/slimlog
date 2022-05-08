/// Creates a hash from a [`str`]
pub fn create_seed(source: &str) -> [u8; 32] {
    use digest::{ VariableOutput, Update };
    let mut blake = blake2::Blake2bVar::new(32).expect("Failed to create blake2 hasher");
    blake.update(source.as_bytes());
    let mut seed: [u8; 32] = [0; 32];
    blake.finalize_variable(seed.as_mut()).expect("Failed to create blake2 hash");
    seed
}
