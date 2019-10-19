#ifndef IR_MODULE_H
#define IR_MODULE_H

#include <string>
#include <vector>

enum IRPortType
{
    IR_INPUT,
    IR_OUTPUT
};

enum IRVariableType
{
    IR_WIRE,
    IR_REGISTER
};

typedef struct
{
    std::string name;
    IRVariableType variableType;
} IRVariable;

typedef struct
{
    IRVariable variable;
    IRPortType portType;
} IRPort;

class IRModule
{
public:
    IRModule() : isTopLevel(false) {}
    void addPort(IRPortType, IRVariableType, const std::string &);
    void addVariable(IRVariableType, const std::string &);
    void dump(std::ostream &);

    void setTopLevel(bool value) { isTopLevel = value; }
    void setModuleName(const char *value) { moduleName = value; }
    void setInstanceName(const char *value) { instanceName = value; }

private:
    bool isTopLevel;
    std::string moduleName;
    std::string instanceName;
    std::vector<IRPort> ports;
    std::vector<IRVariable> variables;
};

#endif