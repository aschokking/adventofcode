def mapl(fn, l):
    return list(map(fn, l))

def filterl(fn, l):
    return list(filter(fn, l))

from itertools import chain
from collections import Counter, defaultdict

from typing import List, Dict, Set, Optional