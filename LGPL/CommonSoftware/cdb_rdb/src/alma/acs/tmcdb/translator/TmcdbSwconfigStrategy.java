package alma.acs.tmcdb.translator;

import org.hibernate.cfg.reveng.ReverseEngineeringStrategy;

public class TmcdbSwconfigStrategy extends AbstractReverseEngineeringStrategy {

	public TmcdbSwconfigStrategy(ReverseEngineeringStrategy delegate) {
		super(delegate);

		// Cannot use reflexion to find classes in a package,
		// so I'll have to hardcode the classnames here.. :(
		columnTranslators = new AbstractColumn2Attribute[] {
				new Column2Attribute_SwCore(),
				new Column2Attribute_SwExt()
		};
		tableTranslators = new AbstractTable2Class[] {
				new Table2Class_SwCore(),
				new Table2Class_SwExt()
		};
		inheritanceTranslators = new AbstractTableInheritance[] {
				new TableInheritance_SwCore(),
				new TableInheritance_SwExt()
		};
	}

}
