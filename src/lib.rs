use std::ops::Range;

// -INFINITE_PENALTY is ok, as -i32::MAX is still in the range of an i32
pub const INFINITE_PENALTY: i32 = i32::MAX;
pub const INFINITE_STRETCHABILITY: i32 = i32::MAX;

#[derive(Clone, Debug)]
pub struct Glue {
    pub width: i32,
    pub shrinkability: i32,
    pub stretchability: i32,
}

impl Glue {
    pub fn new(width: i32, shrinkability: i32, stretchability: i32) -> Self {
        Self {
            width,
            shrinkability,
            stretchability,
        }
    }

    pub fn new_infinite(min_width: i32) -> Self {
        Self {
            width: min_width,
            shrinkability: 0,
            stretchability: INFINITE_STRETCHABILITY,
        }
    }
}

#[derive(Clone, Debug)]
pub struct BoxItem<T> {
    pub width: i32,
    pub user_data: T,
}

impl<T> BoxItem<T> {
    pub fn new(width: i32, user_data: T) -> Self {
        Self { width, user_data }
    }

    /// Applies a transformation to the user data
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> BoxItem<U> {
        let BoxItem { width, user_data } = self;

        BoxItem {
            width,
            user_data: f(user_data),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Penalty<T> {
    pub width: i32,
    pub value: i32,
    /// Called a "flagged" penalty by the Knuth-Plass paper when true
    pub gets_extra_penalty_for_consecutive_lines: bool,
    pub user_data: T,
}

impl<T> Penalty<T> {
    pub fn new(
        width: i32,
        value: i32,
        gets_extra_penalty_for_consecutive_lines: bool,
        user_data: T,
    ) -> Self {
        Self {
            width,
            value,
            gets_extra_penalty_for_consecutive_lines,
            user_data,
        }
    }

    pub fn new_forbid_break(user_data: T) -> Self {
        Self {
            width: 0,
            value: INFINITE_PENALTY,
            gets_extra_penalty_for_consecutive_lines: false,
            user_data,
        }
    }

    pub fn new_forced_break(user_data: T) -> Self {
        Self {
            width: 0,
            value: -INFINITE_PENALTY,
            gets_extra_penalty_for_consecutive_lines: false,
            user_data,
        }
    }

    pub fn is_infinite(&self) -> bool {
        self.value >= INFINITE_PENALTY
    }

    pub fn is_forced_break(&self) -> bool {
        self.value <= -INFINITE_PENALTY
    }

    /// Applies a transformation to the user data
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Penalty<U> {
        let Penalty {
            width,
            value,
            gets_extra_penalty_for_consecutive_lines,
            user_data,
        } = self;

        Penalty {
            width,
            value,
            gets_extra_penalty_for_consecutive_lines,
            user_data: f(user_data),
        }
    }
}

#[derive(Clone, Debug)]
pub enum LineItem<T> {
    Glue(Glue),
    BoxItem(BoxItem<T>),
    Penalty(Penalty<T>),
}

impl<T> LineItem<T> {
    pub fn new_glue(width: i32, shrinkability: i32, stretchability: i32) -> Self {
        LineItem::Glue(Glue::new(width, shrinkability, stretchability))
    }

    pub fn new_infinite_glue(min_width: i32) -> Self {
        LineItem::Glue(Glue::new_infinite(min_width))
    }

    pub fn new_box_item(width: i32, user_data: T) -> Self {
        LineItem::BoxItem(BoxItem::new(width, user_data))
    }

    pub fn new_penalty(
        width: i32,
        value: i32,
        gets_extra_penalty_for_consecutive_lines: bool,
        user_data: T,
    ) -> Self {
        LineItem::Penalty(Penalty::new(
            width,
            value,
            gets_extra_penalty_for_consecutive_lines,
            user_data,
        ))
    }

    pub fn new_forbid_break(user_data: T) -> Self {
        LineItem::Penalty(Penalty::new_forbid_break(user_data))
    }

    pub fn new_forced_break(user_data: T) -> Self {
        LineItem::Penalty(Penalty::new_forced_break(user_data))
    }

    pub fn glue(&self) -> Option<&Glue> {
        if let LineItem::Glue(glue) = self {
            Some(glue)
        } else {
            None
        }
    }

    pub fn box_item(&self) -> Option<&BoxItem<T>> {
        if let LineItem::BoxItem(box_item) = self {
            Some(box_item)
        } else {
            None
        }
    }

    pub fn penalty(&self) -> Option<&Penalty<T>> {
        if let LineItem::Penalty(penalty) = self {
            Some(penalty)
        } else {
            None
        }
    }

    /// Applies a transformation to any user data
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> LineItem<U> {
        match self {
            LineItem::Glue(glue) => LineItem::Glue(glue),
            LineItem::BoxItem(box_item) => LineItem::BoxItem(box_item.map(f)),
            LineItem::Penalty(penalty) => LineItem::Penalty(penalty.map(f)),
        }
    }
}

pub fn break_lines<T>(desired_width: u32, line_items: &[LineItem<T>]) -> Vec<Range<usize>> {
    todo!()
}
