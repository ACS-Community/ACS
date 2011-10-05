package com.cosylab.acs.laser.dao.utils;

import alma.alarmsystem.alarmmessage.generated.AlarmDefinition;
import alma.alarmsystem.alarmmessage.generated.Child;
import alma.alarmsystem.alarmmessage.generated.Parent;
import alma.alarmsystem.alarmmessage.generated.ReductionLinkType;
import alma.alarmsystem.alarmmessage.generated.types.ReductionLinkTypeTypeType;
import cern.laser.business.data.Alarm;

/**
 * <p>Class representing a link specification between the two nodes of a reduction rule,
 * and that is capable of recognizing if a given {@link Alarm} matches the reduction
 * rule definition, considering that the latter might include patterns (e.g.,
 * <code>'a*b'</code>) for both <code>fault-family</code> and <code>fault-member</code>
 * fields on the reduction rule parent node.
 *
 * <p>Reduction rules that include patterns must obey the following rule:
 * <ul>
 * 	<li>For <code>NODE</code> RRs, a single parent (cause) has several children (effects).
 *      Therefore, the RR might include patterns only for the children alarm definition.
 *  <li>For <code>MULTIPLICITY</code> RRs, a single child (synthetic alarm) reduces a set
 *      of alarms that relate somehow (causes). Therefore, the RR might include patterns only
 *      for the parent alarm definition.
 * </ul>
 *
 * <p>This rules does not mean that errors will appear if a pattern expression is present where
 * it shouldn't; no error will occur, but instead the pattern will be treated as an ordinary string.
 * For example, if the user specifies a fault family <code>DV*</code>, it will not match all
 * fault families <code>DV01</code>, <code>DV02</code>, <code>...</code>, but instead it will match
 * the fault family named <code>DV*</code>.
 *
 * @author acaproni, rtobar
 *
 */
public class LinkSpec
{
	public final AlarmRefMatcher _parent, _child;
	public final boolean _isMultiplicity;

	public LinkSpec(ReductionLinkType link)
	{

		Parent p=link.getParent();
		Child c=link.getChild();

		_isMultiplicity = link.getType().equals(ReductionLinkTypeTypeType.MULTIPLICITY);
		_parent         = toMatcher(p.getAlarmDefinition(), _isMultiplicity);
		_child          = toMatcher(c.getAlarmDefinition(), !_isMultiplicity);
	}

	private AlarmRefMatcher toMatcher(AlarmDefinition def, boolean interpretStringsAsPatterns)
	{

		String family=def.getFaultFamily();
		if ( family.length()<1 )
			throw new IllegalArgumentException("Missing fault-family");

		String member=def.getFaultMember();
		if ( member.length()<1 )
			throw new IllegalArgumentException("Missing fault-member");

		int code=def.getFaultCode();
		
		return new AlarmRefMatcher(family, member, code, interpretStringsAsPatterns);
	}

	/**
	 * Reports if a given Alarm matches as a parent, based on the patterns
	 * present in the reduction rule's parent definition.
	 *
	 * @param parent The alarm to be checked as occupying the parent role in the reduction rule 
	 * @return Whether the alarm is parent for the reduction rule or not.
	 */
	public boolean matchParent(Alarm parent)
	{
		return this._parent.isMatch(parent);
	}

	/**
	 * Reports if a given Alarm matches as a child, based on the patterns
	 * present in the reduction rule's child definition.
	 *
	 * @param child The alarm to be checked as occupying the child role in the reduction rule 
	 * @return Whether the alarm is child for the reduction rule or not.
	 */
	public boolean matchChild(Alarm child)
	{
		return this._child.isMatch(child);
	}

	public boolean isMultiplicity()
	{
		return _isMultiplicity;
	}
	
	@Override
	public String toString() {
		StringBuilder ret = new StringBuilder("LinkSpec (ReductionRule): ");
		ret.append("parent=");
		ret.append(_parent.toString());
		ret.append(", child=");
		ret.append(_child.toString());
		return ret.toString();
	}
}