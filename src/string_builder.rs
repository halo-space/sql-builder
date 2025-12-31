//! Lightweight string builder utilities.

#[derive(Debug, Default, Clone)]
pub(crate) struct StringBuilder {
    buf: String,
}

impl StringBuilder {
    pub(crate) fn new() -> Self {
        Self { buf: String::new() }
    }

    /// Write `s`; if not the first write, prepend a space.
    #[allow(dead_code)]
    pub(crate) fn write_leading(&mut self, s: &str) {
        if !self.buf.is_empty() {
            self.buf.push(' ');
        }
        self.buf.push_str(s);
    }

    pub(crate) fn write_str(&mut self, s: &str) {
        self.buf.push_str(s);
    }

    pub(crate) fn write_char(&mut self, c: char) {
        self.buf.push(c);
    }

    #[allow(dead_code)]
    pub(crate) fn write_strings(&mut self, items: &[String], sep: &str) {
        let mut first_added = false;
        for s in items {
            if s.is_empty() {
                continue;
            }
            if first_added {
                self.buf.push_str(sep);
            }
            self.buf.push_str(s);
            first_added = true;
        }
    }

    #[allow(dead_code)]
    pub(crate) fn write_strings_str(&mut self, items: &[&str], sep: &str) {
        let mut first_added = false;
        for &s in items {
            if s.is_empty() {
                continue;
            }
            if first_added {
                self.buf.push_str(sep);
            }
            self.buf.push_str(s);
            first_added = true;
        }
    }

    #[allow(dead_code)]
    pub(crate) fn grow(&mut self, n: usize) {
        self.buf.reserve(n);
    }

    #[allow(dead_code)]
    pub(crate) fn reset(&mut self) {
        self.buf.clear();
    }

    pub(crate) fn into_string(self) -> String {
        self.buf
    }
}

#[allow(dead_code)]
pub(crate) fn filter_empty_strings(mut ss: Vec<String>) -> Vec<String> {
    ss.retain(|s| !s.is_empty());
    ss
}
