use itertools::Itertools;
use std::{cmp::Ordering, collections::BTreeMap, ops::Range};

pub type Value = fixed::types::I32F32;

/// For negative, better use NEG_INFINITE_PENALTY
pub const INFINITE_PENALTY: Value = Value::MAX;
pub const NEG_INFINITE_PENALTY: Value = INFINITE_PENALTY.saturating_neg();

// TODO: Allow adjusting this. Also, this should probably be scaled by the units in use.
const CONSECUTIVE_FLAG_PENALTY: u16 = 3_000;

const MAX_TOLERANCE: u8 = 10;

#[derive(Clone, Debug)]
pub struct Glue {
    pub width: Value,
    pub shrinkability: Value,
    pub stretchability: Value,
}

impl Glue {
    pub fn new(width: Value, shrinkability: Value, stretchability: Value) -> Self {
        Self {
            width,
            shrinkability,
            stretchability,
        }
    }
}

#[derive(Clone, Debug)]
pub struct BoxItem<T> {
    pub width: Value,
    pub user_data: T,
}

impl<T> BoxItem<T> {
    pub fn new(width: Value, user_data: T) -> Self {
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
    pub width: Value,
    pub value: Value,
    /// Called a "flagged" penalty by the Knuth-Plass paper when true
    pub gets_extra_penalty_for_consecutive_lines: bool,
    pub user_data: T,
}

impl<T> Penalty<T> {
    pub fn new(
        width: Value,
        value: Value,
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
            width: 0.into(),
            value: INFINITE_PENALTY,
            gets_extra_penalty_for_consecutive_lines: false,
            user_data,
        }
    }

    pub fn new_forced_break(user_data: T) -> Self {
        Self {
            width: 0.into(),
            value: NEG_INFINITE_PENALTY,
            gets_extra_penalty_for_consecutive_lines: false,
            user_data,
        }
    }

    pub fn is_infinite(&self) -> bool {
        self.value >= INFINITE_PENALTY
    }

    pub fn is_forced_break(&self) -> bool {
        self.value <= NEG_INFINITE_PENALTY
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
    pub fn new_glue(width: Value, shrinkability: Value, stretchability: Value) -> Self {
        LineItem::Glue(Glue::new(width, shrinkability, stretchability))
    }

    pub fn new_box_item(width: Value, user_data: T) -> Self {
        LineItem::BoxItem(BoxItem::new(width, user_data))
    }

    pub fn new_penalty(
        width: Value,
        value: Value,
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

    pub fn user_data(&self) -> Option<&T> {
        match self {
            LineItem::Glue(_) => None,
            LineItem::BoxItem(box_item) => Some(&box_item.user_data),
            LineItem::Penalty(penalty) => Some(&penalty.user_data),
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

fn square(x: Value) -> Value {
    x.saturating_mul(x)
}

fn cube(x: Value) -> Value {
    square(x).saturating_mul(x)
}

/// Get indices without leading penalties and glues
fn trim_line<T>(line_items: &[LineItem<T>], range: Range<usize>) -> Range<usize> {
    let line_items = &line_items[range.clone()];

    if let Some(first_box) = line_items.iter().position(|item| item.box_item().is_some()) {
        debug_assert!(line_items.last().map_or(true, |item| item.glue().is_none()));

        let mut trimmed = first_box..line_items.len();

        // Need to shift by range.start because we used a subset of the original line_items.
        trimmed.start += range.start;
        trimmed.end += range.start;

        return trimmed;
    }

    // Empty range
    0..0
}

#[derive(Clone, Debug)]
struct TotalledValues {
    pub width: Value,
    pub shrinkability: Value,
    pub stretchability: Value,
}

/// Precalculated totals values for an array of LineItems, so that the total of each subarray
/// can be calculated quickly, as described in the paper in the "The Algorithm Itself" section.
#[derive(Debug)]
struct Totals(Vec<TotalledValues>);

impl Totals {
    fn new<T>(line_items: &[LineItem<T>]) -> Self {
        let initial = TotalledValues {
            width: 0.into(),
            shrinkability: 0.into(),
            stretchability: 0.into(),
        };

        let values = line_items
            .iter()
            .scan(initial, |total, item| {
                let total_before_item = total.clone();

                total.width = total
                    .width
                    .checked_add(match item {
                        LineItem::BoxItem(b) => b.width,
                        LineItem::Glue(g) => g.width,
                        LineItem::Penalty(_) => 0.into(),
                    })
                    .unwrap();

                if let LineItem::Glue(g) = item {
                    total.shrinkability = total.shrinkability.checked_add(g.shrinkability).unwrap();
                    total.stretchability =
                        total.stretchability.checked_add(g.stretchability).unwrap();
                }

                Some(total_before_item)
            })
            .collect();

        Self(values)
    }

    /// Get total values for items in range. The given range must *not* include the last LineItem.
    fn get(&self, range: Range<usize>) -> TotalledValues {
        let first = &self.0[range.start];
        // Totals are precalculated *before* each item, so to include the last item in the range
        // in our total, we need to get the precalculated totals before the *next* item. `range`
        // was an exclusive range, so the next item is at `range.end`.
        let after_last = &self.0[range.end];

        TotalledValues {
            width: after_last.width.checked_sub(first.width).unwrap(),
            shrinkability: after_last
                .shrinkability
                .checked_sub(first.shrinkability)
                .unwrap(),
            stretchability: after_last
                .stretchability
                .checked_sub(first.stretchability)
                .unwrap(),
        }
    }
}

/// Returns None if the adjustment ratio is undefined
fn calc_adjustment_ratio<T>(
    desired_width: Value,
    line_items: &[LineItem<T>],
    totals: &Totals,
    range: Range<usize>,
    next_item: &LineItem<T>,
) -> Option<Value> {
    let trailing_penalty = next_item.penalty();

    let trimmed_range = trim_line(line_items, range);
    let line_items = &line_items[trimmed_range.clone()];
    if line_items.is_empty() {
        return None;
    }

    // `get()` requires that `trimmed_range` not include the last item in line_items, but we never
    // will, because this function is always called with a `next_item`.
    let totals = totals.get(trimmed_range);

    let mut total_width = totals.width;
    if let Some(p) = trailing_penalty {
        total_width = total_width.saturating_add(p.width);
    }

    let diff = desired_width.saturating_sub(total_width);

    match diff.cmp(&0.into()) {
        // desired_width == total_width
        Ordering::Equal => Some(0.into()),

        // desired_width > total_width, need to stretch
        Ordering::Greater => {
            if totals.stretchability > 0 {
                Some(diff / totals.stretchability)
            } else {
                None
            }
        }

        // desired_width < total_width, need to shrink
        Ordering::Less => {
            if totals.shrinkability > 0 {
                Some(diff / totals.shrinkability)
            } else {
                None
            }
        }
    }
}

/// adj_ratio should be defined and >= -1
fn calc_line_badness(adj_ratio: Value) -> Value {
    debug_assert!(adj_ratio >= -1);

    // TODO: This should probably be scaled by the units in use.
    Value::from(100).saturating_mul(cube(adj_ratio))
}

fn calc_line_demerits<T>(
    adj_ratio: Value,
    next_item: &LineItem<T>,
    last_line_had_flagged_penalty: bool,
) -> Value {
    let trailing_penalty = next_item.penalty();

    let alpha = if last_line_had_flagged_penalty
        && trailing_penalty.map(|p| p.gets_extra_penalty_for_consecutive_lines) == Some(true)
    {
        Value::from(CONSECUTIVE_FLAG_PENALTY)
    } else {
        0.into()
    };

    let penalty_value = Value::from(trailing_penalty.map(|p| p.value).unwrap_or(0.into()));
    let badness = calc_line_badness(adj_ratio);

    if penalty_value >= 0 {
        square(
            Value::from(1)
                .saturating_add(badness)
                .saturating_add(penalty_value),
        )
        .saturating_add(alpha)
    } else if -INFINITE_PENALTY < penalty_value {
        square(Value::from(1).saturating_add(badness))
            .saturating_sub(penalty_value)
            .saturating_add(alpha)
    } else {
        square(Value::from(1).saturating_add(badness)).saturating_add(alpha)
    }
}

#[derive(Debug)]
struct BreakpointInfo {
    previous_breakpoint: usize,
    adj_ratio_for_last_line: Value,
    demerits_from_start_of_paragraph: Value,
}

fn is_legal_breakpoint<T>(prev_item: &LineItem<T>, this_item: &LineItem<T>) -> bool {
    match (prev_item, this_item) {
        (_, LineItem::Penalty(p)) => p.value < INFINITE_PENALTY,
        (LineItem::BoxItem(_), LineItem::Glue(_)) => true,
        _ => false,
    }
}

pub struct Line {
    pub range: Range<usize>,
    pub adj_ratio: Value,
}

fn break_lines_with_tolerance<T>(
    desired_width: Value,
    line_items: &[LineItem<T>],
    tolerance: Value,
) -> Option<Vec<Line>> {
    assert!(!line_items.is_empty());

    let totals = Totals::new(line_items);

    let mut breakpoint_infos: BTreeMap<usize, BreakpointInfo> = BTreeMap::new();
    // previous won't actually be used
    breakpoint_infos.insert(
        0,
        BreakpointInfo {
            previous_breakpoint: 0,
            adj_ratio_for_last_line: 0.into(),
            demerits_from_start_of_paragraph: 0.into(),
        },
    );

    let mut active_breakpoints = vec![0];

    // TODO: force break after line_items
    for (prev_i, (prev_item, cur_item)) in line_items.iter().tuple_windows().enumerate() {
        if !is_legal_breakpoint(prev_item, cur_item) {
            continue;
        }

        let i = prev_i + 1;

        let is_forced = cur_item
            .penalty()
            .map(|p| p.is_forced_break())
            .unwrap_or(false);

        struct ScoredActiveBreakpoint {
            index: usize,
            adj_ratio: Value,
            demerits: Value,
        }

        let scored = active_breakpoints.iter().copied().filter_map(|a| {
            let a_info = &breakpoint_infos[&a];

            if let Some(adj_ratio) =
                calc_adjustment_ratio(desired_width, &line_items, &totals, a..i, cur_item)
            {
                if !(-1 <= adj_ratio && adj_ratio <= tolerance) {
                    return None;
                }

                // TODO: flag from previous line
                let line_demerits = calc_line_demerits(adj_ratio, cur_item, false);
                let total_demerits = a_info
                    .demerits_from_start_of_paragraph
                    .saturating_add(line_demerits);

                Some(ScoredActiveBreakpoint {
                    index: a,
                    adj_ratio,
                    demerits: total_demerits,
                })
            } else {
                None
            }
        });

        if let Some(best) = scored.min_by_key(|a| a.demerits) {
            breakpoint_infos.insert(
                i,
                BreakpointInfo {
                    previous_breakpoint: best.index,
                    adj_ratio_for_last_line: best.adj_ratio,
                    demerits_from_start_of_paragraph: if is_forced {
                        // New paragraph
                        0.into()
                    } else {
                        best.demerits
                    },
                },
            );

            if is_forced {
                active_breakpoints.clear();
            }

            active_breakpoints.push(i);
        } else if is_forced {
            return None;
        }
    }

    let mut final_indices = vec![];
    // Already checked that line_items isn't empty
    let mut index = line_items.len() - 1;
    while index > 0 {
        final_indices.push(index);

        index = breakpoint_infos[&index].previous_breakpoint;
    }
    final_indices.push(0);

    // Now get line indices
    Some(
        final_indices
            .into_iter()
            .rev()
            .tuple_windows()
            .map(|(a, b)| {
                // TODO: include trailing penalty in returned line
                let range = trim_line(line_items, a..b);
                let adj_ratio = breakpoint_infos[&b].adj_ratio_for_last_line;

                Line { range, adj_ratio }
            })
            .collect(),
    )
}

pub fn break_lines<T>(desired_width: Value, line_items: &[LineItem<T>]) -> Vec<Line> {
    for tolerance in 1..=MAX_TOLERANCE {
        if let Some(result) =
            break_lines_with_tolerance(desired_width, line_items, tolerance.into())
        {
            return result;
        }
    }

    panic!("Failed to break lines with tolerance <= {}", MAX_TOLERANCE);
}
