#ifndef ARGINFO_HPP
#define ARGINFO_HPP

class ArgInfo
{
private:
    static const uint32_t arg_outputarg_flag     = 0x1;
    static const uint32_t arg_arithm_op_src_flag = 0x2;
    static const uint32_t arg_pathlike_flag      = 0x4;
    static const uint32_t arg_has_default_flag   = 0x8;

public:
    const char* name;
    bool outputarg;
    bool arithm_op_src;
    bool pathlike;
    bool has_default; // <- added in evision
    // more fields may be added if necessary

    ArgInfo(const char* name_, uint32_t arg_) :
        name(name_),
        outputarg((arg_ & arg_outputarg_flag) != 0),
        arithm_op_src((arg_ & arg_arithm_op_src_flag) != 0),
        pathlike((arg_ & arg_pathlike_flag) != 0),
        has_default((arg_ & arg_has_default_flag) != 0) {}

private:
    ArgInfo(const ArgInfo&) = delete;
    ArgInfo& operator=(const ArgInfo&) = delete;
};

#endif  // ARGINFO_HPP
