# ifndef LABEL_H
# define LABEL_H

#include <ostream>

class Label{
  static unsigned _counter;
  unsigned _number;

public:
  Label();
  unsigned number() const;
};

ostream &operator <<(std::ostream &ostr, const Label &label);
//std:ostream &operator <<(std::ostream &ostr, const Label &label);


# endif