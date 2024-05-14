pub trait FullName {
    fn full_name(&self) -> String;
}

impl FullName for String {
    fn full_name(&self) -> String {
        self.clone()
    }
}

impl FullName for &str {
    fn full_name(&self) -> String {
        self.to_string()
    }
}

impl<T: FullName> FullName for &T {
    fn full_name(&self) -> String {
        (*self).full_name()
    }
}
