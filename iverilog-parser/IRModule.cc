#include "IRModule.h"

void IRModule::addPort(IRPortType portType, IRVariableType variableType, const std::string &variableName)
{
    IRPort port;
    IRVariable variable;
    port.portType = portType;
    port.variable.variableType = variableType;
    port.variable.name = variableName;
    ports.push_back(port);
}

void IRModule::addVariable(IRVariableType variableType, const std::string &variableName)
{
    IRVariable variable;
    variable.variableType = variableType;
    variable.name = variableName;
    variables.push_back(variable);
}

void IRModule::dump(std::ostream &)
{
    // TODO
}