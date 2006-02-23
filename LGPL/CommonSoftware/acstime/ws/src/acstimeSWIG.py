# This file was created automatically by SWIG.
# Don't modify this file, modify the SWIG interface instead.
# This file is compatible with both classic and new-style classes.

import _acstimeSWIG

def _swig_setattr(self,class_type,name,value):
    if (name == "this"):
        if isinstance(value, class_type):
            self.__dict__[name] = value.this
            if hasattr(value,"thisown"): self.__dict__["thisown"] = value.thisown
            del value.thisown
            return
    method = class_type.__swig_setmethods__.get(name,None)
    if method: return method(self,value)
    self.__dict__[name] = value

def _swig_getattr(self,class_type,name):
    method = class_type.__swig_getmethods__.get(name,None)
    if method: return method(self)
    raise AttributeError,name

import types
try:
    _object = types.ObjectType
    _newclass = 1
except AttributeError:
    class _object : pass
    _newclass = 0
del types


TCEqualTo = _acstimeSWIG.TCEqualTo
TCLessThan = _acstimeSWIG.TCLessThan
TCGreaterThan = _acstimeSWIG.TCGreaterThan
TCIndeterminate = _acstimeSWIG.TCIndeterminate
TSArray = _acstimeSWIG.TSArray
TSTAI = _acstimeSWIG.TSTAI
TSUTC = _acstimeSWIG.TSUTC
class Duration(_object):
    __swig_setmethods__ = {}
    __setattr__ = lambda self, name, value: _swig_setattr(self, Duration, name, value)
    __swig_getmethods__ = {}
    __getattr__ = lambda self, name: _swig_getattr(self, Duration, name)
    def __repr__(self):
        return "<C acstime::Duration instance at %s>" % (self.this,)
    __swig_setmethods__["value"] = _acstimeSWIG.Duration_value_set
    __swig_getmethods__["value"] = _acstimeSWIG.Duration_value_get
    if _newclass:value = property(_acstimeSWIG.Duration_value_get, _acstimeSWIG.Duration_value_set)
    def __init__(self, *args):
        _swig_setattr(self, Duration, 'this', _acstimeSWIG.new_Duration(*args))
        _swig_setattr(self, Duration, 'thisown', 1)
    def __del__(self, destroy=_acstimeSWIG.delete_Duration):
        try:
            if self.thisown: destroy(self)
        except: pass

class DurationPtr(Duration):
    def __init__(self, this):
        _swig_setattr(self, Duration, 'this', this)
        if not hasattr(self,"thisown"): _swig_setattr(self, Duration, 'thisown', 0)
        _swig_setattr(self, Duration,self.__class__,Duration)
_acstimeSWIG.Duration_swigregister(DurationPtr)

class Epoch(_object):
    __swig_setmethods__ = {}
    __setattr__ = lambda self, name, value: _swig_setattr(self, Epoch, name, value)
    __swig_getmethods__ = {}
    __getattr__ = lambda self, name: _swig_getattr(self, Epoch, name)
    def __repr__(self):
        return "<C acstime::Epoch instance at %s>" % (self.this,)
    __swig_setmethods__["value"] = _acstimeSWIG.Epoch_value_set
    __swig_getmethods__["value"] = _acstimeSWIG.Epoch_value_get
    if _newclass:value = property(_acstimeSWIG.Epoch_value_get, _acstimeSWIG.Epoch_value_set)
    def __init__(self, *args):
        _swig_setattr(self, Epoch, 'this', _acstimeSWIG.new_Epoch(*args))
        _swig_setattr(self, Epoch, 'thisown', 1)
    def __del__(self, destroy=_acstimeSWIG.delete_Epoch):
        try:
            if self.thisown: destroy(self)
        except: pass

class EpochPtr(Epoch):
    def __init__(self, this):
        _swig_setattr(self, Epoch, 'this', this)
        if not hasattr(self,"thisown"): _swig_setattr(self, Epoch, 'thisown', 0)
        _swig_setattr(self, Epoch,self.__class__,Epoch)
_acstimeSWIG.Epoch_swigregister(EpochPtr)

class TimeUtil(_object):
    __swig_setmethods__ = {}
    __setattr__ = lambda self, name, value: _swig_setattr(self, TimeUtil, name, value)
    __swig_getmethods__ = {}
    __getattr__ = lambda self, name: _swig_getattr(self, TimeUtil, name)
    def __repr__(self):
        return "<C TimeUtil instance at %s>" % (self.this,)
    def __init__(self, *args):
        _swig_setattr(self, TimeUtil, 'this', _acstimeSWIG.new_TimeUtil(*args))
        _swig_setattr(self, TimeUtil, 'thisown', 1)
    def __del__(self, destroy=_acstimeSWIG.delete_TimeUtil):
        try:
            if self.thisown: destroy(self)
        except: pass

class TimeUtilPtr(TimeUtil):
    def __init__(self, this):
        _swig_setattr(self, TimeUtil, 'this', this)
        if not hasattr(self,"thisown"): _swig_setattr(self, TimeUtil, 'thisown', 0)
        _swig_setattr(self, TimeUtil,self.__class__,TimeUtil)
_acstimeSWIG.TimeUtil_swigregister(TimeUtilPtr)

class DurationHelper(_object):
    __swig_setmethods__ = {}
    __setattr__ = lambda self, name, value: _swig_setattr(self, DurationHelper, name, value)
    __swig_getmethods__ = {}
    __getattr__ = lambda self, name: _swig_getattr(self, DurationHelper, name)
    def __repr__(self):
        return "<C DurationHelper instance at %s>" % (self.this,)
    def __init__(self, *args):
        _swig_setattr(self, DurationHelper, 'this', _acstimeSWIG.new_DurationHelper(*args))
        _swig_setattr(self, DurationHelper, 'thisown', 1)
    def __del__(self, destroy=_acstimeSWIG.delete_DurationHelper):
        try:
            if self.thisown: destroy(self)
        except: pass
    def value(*args): return _acstimeSWIG.DurationHelper_value(*args)
    def positive(*args): return _acstimeSWIG.DurationHelper_positive(*args)
    def day(*args): return _acstimeSWIG.DurationHelper_day(*args)
    def hour(*args): return _acstimeSWIG.DurationHelper_hour(*args)
    def minute(*args): return _acstimeSWIG.DurationHelper_minute(*args)
    def second(*args): return _acstimeSWIG.DurationHelper_second(*args)
    def microSecond(*args): return _acstimeSWIG.DurationHelper_microSecond(*args)
    def normalize(*args): return _acstimeSWIG.DurationHelper_normalize(*args)
    def reset(*args): return _acstimeSWIG.DurationHelper_reset(*args)
    def compare(*args): return _acstimeSWIG.DurationHelper_compare(*args)
    def add(*args): return _acstimeSWIG.DurationHelper_add(*args)
    def __iadd__(*args): return _acstimeSWIG.DurationHelper___iadd__(*args)
    def subtract(*args): return _acstimeSWIG.DurationHelper_subtract(*args)
    def __isub__(*args): return _acstimeSWIG.DurationHelper___isub__(*args)
    def modulo(*args): return _acstimeSWIG.DurationHelper_modulo(*args)
    def __imod__(*args): return _acstimeSWIG.DurationHelper___imod__(*args)
    def multiply(*args): return _acstimeSWIG.DurationHelper_multiply(*args)
    def __imul__(*args): return _acstimeSWIG.DurationHelper___imul__(*args)
    def divide(*args): return _acstimeSWIG.DurationHelper_divide(*args)
    def __idiv__(*args): return _acstimeSWIG.DurationHelper___idiv__(*args)
    def toString(*args): return _acstimeSWIG.DurationHelper_toString(*args)
    def fromString(*args): return _acstimeSWIG.DurationHelper_fromString(*args)

class DurationHelperPtr(DurationHelper):
    def __init__(self, this):
        _swig_setattr(self, DurationHelper, 'this', this)
        if not hasattr(self,"thisown"): _swig_setattr(self, DurationHelper, 'thisown', 0)
        _swig_setattr(self, DurationHelper,self.__class__,DurationHelper)
_acstimeSWIG.DurationHelper_swigregister(DurationHelperPtr)

class EpochHelper(_object):
    __swig_setmethods__ = {}
    __setattr__ = lambda self, name, value: _swig_setattr(self, EpochHelper, name, value)
    __swig_getmethods__ = {}
    __getattr__ = lambda self, name: _swig_getattr(self, EpochHelper, name)
    def __repr__(self):
        return "<C EpochHelper instance at %s>" % (self.this,)
    def __init__(self, *args):
        _swig_setattr(self, EpochHelper, 'this', _acstimeSWIG.new_EpochHelper(*args))
        _swig_setattr(self, EpochHelper, 'thisown', 1)
    def __del__(self, destroy=_acstimeSWIG.delete_EpochHelper):
        try:
            if self.thisown: destroy(self)
        except: pass
    def value(*args): return _acstimeSWIG.EpochHelper_value(*args)
    def year(*args): return _acstimeSWIG.EpochHelper_year(*args)
    def month(*args): return _acstimeSWIG.EpochHelper_month(*args)
    def day(*args): return _acstimeSWIG.EpochHelper_day(*args)
    def dayOfYear(*args): return _acstimeSWIG.EpochHelper_dayOfYear(*args)
    def dayOfWeek(*args): return _acstimeSWIG.EpochHelper_dayOfWeek(*args)
    def hour(*args): return _acstimeSWIG.EpochHelper_hour(*args)
    def minute(*args): return _acstimeSWIG.EpochHelper_minute(*args)
    def second(*args): return _acstimeSWIG.EpochHelper_second(*args)
    def microSecond(*args): return _acstimeSWIG.EpochHelper_microSecond(*args)
    def normalize(*args): return _acstimeSWIG.EpochHelper_normalize(*args)
    def reset(*args): return _acstimeSWIG.EpochHelper_reset(*args)
    def compare(*args): return _acstimeSWIG.EpochHelper_compare(*args)
    def add(*args): return _acstimeSWIG.EpochHelper_add(*args)
    def subtract(*args): return _acstimeSWIG.EpochHelper_subtract(*args)
    def difference(*args): return _acstimeSWIG.EpochHelper_difference(*args)
    def toUTCdate(*args): return _acstimeSWIG.EpochHelper_toUTCdate(*args)
    def toJulianYear(*args): return _acstimeSWIG.EpochHelper_toJulianYear(*args)
    def toString(*args): return _acstimeSWIG.EpochHelper_toString(*args)
    def fromString(*args): return _acstimeSWIG.EpochHelper_fromString(*args)

class EpochHelperPtr(EpochHelper):
    def __init__(self, this):
        _swig_setattr(self, EpochHelper, 'this', this)
        if not hasattr(self,"thisown"): _swig_setattr(self, EpochHelper, 'thisown', 0)
        _swig_setattr(self, EpochHelper,self.__class__,EpochHelper)
_acstimeSWIG.EpochHelper_swigregister(EpochHelperPtr)


