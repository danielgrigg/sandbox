 struct transition_table : 
    mpl::vector<
    Row<Look, fill_detect, Fill, none, my_guard> >,

    Row<Fill, swing_detect, MaybeSwing, none, none> >,
    Row<Fill, fill_timeout, Look, none, none> >,

    Row<MaybeSwing, min_swing_timeout, Swing, none, none> >,
    Row<MaybeSwing, swing_timeout, Look, none, none> >,

    Row<Swing, swing_timeout, Look, none, none> >,
    Row<Swing, dump_detect, SwingDump, none, none> >,
    Row<Swing, return_detect, Return, none, none> >,

    Row<SwingDump, swing_timeout, Look, none, none> >,
    Row<SwingDump, idle_detect, Look, none, none> >,
    Row<SwingDump, fill_detect, Look, none, none> >,

    Row<Return, fill_detect, Fill, none, none> >,
    Row<Return, return_timeout, Look, on_return_timeout, none> >,
    Row<Return, return_end, Look, on_return_end, none> >,
    Row<Return, idle_detect, Look, on_return_idle, none> >
    {};

