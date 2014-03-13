#ifndef MC_DEVIO_H
#define MC_DEVIO_H
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2009
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: MCtestDevIO.h,v 1.2 2011/05/23 19:31:44 javarias Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2009-02-11  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baciDevIO.h>

namespace TMCDB
{

template <class T> class MCtestDevIO : public DevIO<T>
{
 public:

    /**
     * Constructor that accepts a reference to an object of any type using templates.
     * \param value templated reference to the allocated memory to be used for the data
     * \param init timestamp
     */
	 MCtestDevIO(T &value, ACS::Time& timestamp) :
		 value_m(value),
		 timestamp_m(timestamp)
	 {};

    virtual ~MCtestDevIO() {};

    virtual bool initializeValue(){ return true; }

    /**
     *  @throw ACSErr::ACSbaseExImpl
     */
    virtual T read(ACS::Time& timestamp)
	{
    	timestamp_m+= 10000000; //we increase timestamp for 1sec
    	value_m++;

    	timestamp = timestamp_m;
	    return value_m;
	}

    /**
     *  @throw ACSErr::ACSbaseExImpl
     */
    virtual void write(const T& value, ACS::Time& timestamp)
	{
	    timestamp = timestamp_m;
	    value_m = value;
	}

  protected:
    T value_m;
    ACS::Time timestamp_m;
};

template <class T> class MCtestDevIOSeq : public DevIO<T>
{
public:

	/**
	 * Constructor that accepts a reference to an object of any type using templates.
	 * \param value templated reference to the allocated memory to be used for the data
	 * \param init timestamp
	 */
	/**
	 * Constructor that accepts a reference to an object of any type using templates.
	 * \param value templated reference to the allocated memory to be used for the data
	 * \param init timestamp
	 */
	MCtestDevIOSeq(T &value, ACS::Time& timestamp) :
		value_m(value),
		timestamp_m(timestamp)
		{};

		virtual ~MCtestDevIOSeq() {};

		virtual bool initializeValue(){ return true; }



		T read(ACS::Time& timestamp)
		{
			for(unsigned int i=0;i<this->value_m.length();i++)
				this->value_m[i]++;

			this->timestamp_m+= 10000000; //we increase timestamp for 1sec

			timestamp = this->timestamp_m;
			return this->value_m;
		}

		virtual void write(const T& value, ACS::Time& timestamp)
		{
			timestamp_m = timestamp;
			value_m = value;
		}

protected:
	T value_m;
	ACS::Time timestamp_m;
};

	template <class T> class MCtestDevIONoIncremental : public DevIO<T>
	{
	  public:
	
		/**
		 * Constructor that accepts a reference to an object of any type using templates.
		 * \param value templated reference to the allocated memory to be used for the data
		 * \param init timestamp
		 */
		MCtestDevIONoIncremental(T &value, ACS::Time& timestamp) :
			value_m(value),
			timestamp_m(timestamp)
		{}
	
		virtual ~MCtestDevIONoIncremental() {};
		virtual bool initializeValue(){ return true; }
	
		/**
		 *  @throw ACSErr::ACSbaseExImpl
		 */
		virtual T read(ACS::Time& timestamp)
		{
			timestamp = timestamp_m;
			return value_m;
		}
	
		 /**
		  *  @throw ACSErr::ACSbaseExImpl
		  */
		 virtual void write(const T& value, ACS::Time& timestamp)
		{
			timestamp_m = timestamp;
			value_m = value;
		}
	
	  protected:
		T value_m;
		ACS::Time timestamp_m;
	};
	
	template <class T> class MCtestDevIOSeqNoIncremental : public DevIO<T>
	{
	public:
	
		/**
		 * Constructor that accepts a reference to an object of any type using templates.
		 * \param value templated reference to the allocated memory to be used for the data
		 * \param init timestamp
		 */
		MCtestDevIOSeqNoIncremental(T &value, ACS::Time& timestamp) :
			value_m(value),
			timestamp_m(timestamp)
			{}

			virtual ~MCtestDevIOSeqNoIncremental() {};
			virtual bool initializeValue(){ return true; }
	
	
	
			T read(ACS::Time& timestamp)
			{
				timestamp = this->timestamp_m;
				return this->value_m;
			}
	
			virtual void write(const T& value, ACS::Time& timestamp)
			{
				timestamp_m = timestamp;
				value_m = value;
			}
	
	protected:
		T value_m;
		ACS::Time timestamp_m;
	};

};//namespace TMCDB

#endif /*!_H*/
