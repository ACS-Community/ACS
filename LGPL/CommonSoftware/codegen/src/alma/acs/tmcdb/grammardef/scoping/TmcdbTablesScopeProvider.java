package alma.acs.tmcdb.grammardef.scoping;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.ecore.EReference;
import org.eclipse.xtext.resource.EObjectDescription;
import org.eclipse.xtext.resource.IEObjectDescription;
import org.eclipse.xtext.scoping.IScope;
import org.eclipse.xtext.scoping.impl.AbstractDeclarativeScopeProvider;
import org.eclipse.xtext.scoping.impl.SimpleScope;

import alma.acs.tmcdb.grammardef.tmcdbTables.CheckConstraint;
import alma.acs.tmcdb.grammardef.tmcdbTables.Column;
import alma.acs.tmcdb.grammardef.tmcdbTables.ForeignConstraint;
import alma.acs.tmcdb.grammardef.tmcdbTables.Key;
import alma.acs.tmcdb.grammardef.tmcdbTables.Table;
import alma.acs.tmcdb.grammardef.tmcdbTables.UniquenessConstraint;

/**
 * This class contains custom scoping description.
 * 
 * see : http://www.eclipse.org/Xtext/documentation/latest/xtext.html#scoping
 * on how and when to use it 
 *
 */
public class TmcdbTablesScopeProvider extends AbstractDeclarativeScopeProvider {

	IScope scope_Key_columns(Key ctx, EReference ref) {
		SimpleScope scope = new SimpleScope(IScope.NULLSCOPE, getColumnsAsIScopedElements((Table)ctx.eContainer()) );
		return scope;
	}

	IScope scope_Key_pieces(Key ctx, EReference ref) {
		SimpleScope scope = new SimpleScope(IScope.NULLSCOPE, getColumnsAsIScopedElements((Table)ctx.eContainer()) );
		return scope;
	}

	IScope scope_ForeignConstraint_columns(ForeignConstraint ctx, EReference ref) {
		SimpleScope scope = new SimpleScope(IScope.NULLSCOPE, getColumnsAsIScopedElementsWithInheritance((Table)ctx.eContainer()) );
		return scope;
	}

	IScope scope_CheckConstraint_column(CheckConstraint ctx, EReference ref) {
		SimpleScope scope = new SimpleScope(IScope.NULLSCOPE, getColumnsAsIScopedElementsWithInheritance((Table)ctx.eContainer()) );
		return scope;
	}

	IScope scope_ForeignConstraint_foreignColumn(ForeignConstraint ctx, EReference ref) {
		SimpleScope scope = new SimpleScope(IScope.NULLSCOPE, getColumnsAsIScopedElementsWithInheritance(ctx.getTable()) );
		return scope;
	}

	IScope scope_UniquenessConstraint_columns(UniquenessConstraint ctx, EReference ref) {
		SimpleScope scope = new SimpleScope(IScope.NULLSCOPE, getColumnsAsIScopedElementsWithInheritance((Table)ctx.eContainer()) );
		return scope;
	}

	private Iterable<IEObjectDescription> getColumnsAsIScopedElements(
			Table table) {

		List<IEObjectDescription> result = new ArrayList<IEObjectDescription>();

		for(Column c: table.getColumns())
			result.add(EObjectDescription.create(c.getName(), c));

		return result;
	}

	private Iterable<IEObjectDescription> getColumnsAsIScopedElementsWithInheritance(
			Table table) {

		List<IEObjectDescription> result = new ArrayList<IEObjectDescription>();

		Table t = table;
		do {
			for(Column c: t.getColumns())
				result.add(EObjectDescription.create(c.getName(), c));
		} while( (t = t.getSuperTable()) != null );

		return result;
	}
}