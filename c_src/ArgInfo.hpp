#ifndef ARGINFO_HPP
#define ARGINFO_HPP

class ArgInfo
{
private:
    static const uint32_t arg_outputarg_flag     = 0x1;
    static const uint32_t arg_arithm_op_src_flag = 0x2;
    static const uint32_t arg_pathlike_flag      = 0x4;
    static const uint32_t arg_nd_mat_flag        = 0x8;
    static const uint32_t arg_has_default_flag   = 0x10;
    static const uint32_t arg_input_only_flag    = 0x20;

public:
    const char* name;
    bool outputarg;
    bool arithm_op_src;
    bool pathlike;
    bool nd_mat;
    bool has_default; // <- added in evision
    // Pure read-only input. Set by generated bindings (const InputArray) and by
    // hand-written modules that only read a source matrix (clone-then-work or
    // write-a-fresh-dst). It lets converters share a cv-owned source buffer
    // instead of deep-copying it; binary-backed sources still deep-copy (see the
    // in_buf guard in evision_to), so sharing can never outlive an Erlang binary.
    bool input_only;  // <- added in evision
    // more fields may be added if necessary

    // Flag value for hand-written modules: ArgInfo("src", ArgInfo::INPUT_ONLY).
    static const uint32_t INPUT_ONLY = arg_input_only_flag;

    ArgInfo(const char* name_, uint32_t arg_) :
        name(name_),
        outputarg((arg_ & arg_outputarg_flag) != 0),
        arithm_op_src((arg_ & arg_arithm_op_src_flag) != 0),
        pathlike((arg_ & arg_pathlike_flag) != 0),
        nd_mat((arg_ & arg_nd_mat_flag) != 0),
        has_default((arg_ & arg_has_default_flag) != 0),
        input_only((arg_ & arg_input_only_flag) != 0) {}

private:
    ArgInfo(const ArgInfo&) = delete;
    ArgInfo& operator=(const ArgInfo&) = delete;
};

#endif  // ARGINFO_HPP
