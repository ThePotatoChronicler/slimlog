use log::{ warn };

const DEFAULT_HEX_VAR_LENGTH: u8 = 6;

/// Compiler settings
pub struct Settings {
    /// Use random hexadecimal names instead of sequential names
    hex_unnamed_vars: bool,
    /// Length of random hexadecimal names (max 32)
    hex_var_length: u8,
    /// Enables translational optimizations
    transopts: bool,
}

impl Settings {
    /// Creates a new unfinalized Settings struct with default settings
    pub fn new() -> Self {
        Self {
            hex_unnamed_vars: true,
            hex_var_length: DEFAULT_HEX_VAR_LENGTH,
            transopts: false,
        }
    }

    /// Use random hexadecimal variable names
    pub fn hex_unnamed_vars(&mut self, hex_unnamed_vars: bool) -> &mut Self {
        self.hex_unnamed_vars = hex_unnamed_vars;
        self
    }

    pub fn get_hex_unnamed_vars(&self) -> bool {
        self.hex_unnamed_vars
    }

    /// Use translation optimizations
    pub fn transopts(&mut self, transopts: bool) -> &mut Self {
        self.transopts = transopts;
        self
    }

    pub fn get_transopts(&self) -> bool {
        self.transopts
    }

    /// Length of random hexadecimal variable names
    pub fn hex_var_length(&mut self, mut hex_var_length: u8) -> &mut Self {
        if hex_var_length > 32 {
            warn!("`hex_var_length' set to a value higher than 32, clamping to 32");
            hex_var_length = 32;
        }

        if hex_var_length == 0 {
            warn!("`hex_var_length' cannot be 0, setting to default");
            hex_var_length = DEFAULT_HEX_VAR_LENGTH;
        }

        if hex_var_length <= 4 {
            warn!("`hex_var_length' shouldn't be less than 4");
        }

        self.hex_var_length = hex_var_length;
        self
    }

    pub fn get_hex_var_length(&self) -> u8 {
        self.hex_var_length
    }
}
