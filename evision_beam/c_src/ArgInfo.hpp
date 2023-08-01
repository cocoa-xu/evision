#ifndef ARGINFO_HPP
#define ARGINFO_HPP

class ArgInfo
{
public:
    const char* name;
    bool outputarg;
    bool has_default;
    // more fields may be added if necessary

    ArgInfo(const char* name_, bool outputarg_, bool has_default_ = false) : 
    name(name_), outputarg(outputarg_), has_default(has_default_) {}

private:
    ArgInfo(const ArgInfo&) = delete;
    ArgInfo& operator=(const ArgInfo&) = delete;
};

#endif // ARGINFO_HPP
