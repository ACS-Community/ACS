package alma.ACS.jbaci;

//cmenay
//reimplementation of the C++ acsutilWildcard.cpp created by Matej.
public class UtilsWildcards
{
	
	
	public boolean asterisk(String test, String wildcard,int wildcardPos,int i){
		int j;
		if (wildcardPos >= (wildcard.length() - 1))
			return true;
		
		for (j = i; j < test.length(); j++)
		{
			if (wildcardFit(test.substring(j), wildcard.substring(wildcardPos + 1)))
				return true;
		}
		return false;
	}
	
	public boolean set(String test, String wildcard, int wildcardPos, int i, char thisChar){
		char lastwildcardChar;
		char wildcardChar;
		int j;
		
		if (wildcardPos >= (wildcard.length() - 1))
			return false;
		
		lastwildcardChar = '\u0000';
		for (j = wildcardPos + 1; j < wildcard.length(); j++)
		{
			wildcardChar = wildcard.charAt(j);
			if (wildcardChar == ']')
			{
				return false;
			}  else	if (wildcardChar == '-')  {
				j++;
				if (j == wildcard.length())
					return false;
				
				wildcardChar = wildcard.charAt(j);
				if (wildcardChar == ']')
				{
					return false;	
				}  else  {
					if ((thisChar >= lastwildcardChar) && (thisChar <= wildcardChar))
						break;
				}
			}  else if (thisChar == wildcardChar)  {
				break;
			}
			
			lastwildcardChar = wildcardChar;
		}
		wildcardPos = j;
		for (j = wildcardPos; j < wildcard.length(); j++)
		{
			if (wildcard.charAt(j) == ']')
				break;
		}
		wildcardPos = j;
		return true;
		
		
	}
	public boolean wildcardFit (String test, String wildcard)
	{
		char wildcardChar;
		int wildcardPos = 0;
		char thisChar;
		int i, j;
		
		for (i = 0; i < test.length(); i++)
		{
			if (wildcardPos >= wildcard.length())
				return false;

			wildcardChar = wildcard.charAt(wildcardPos);
			thisChar = test.charAt(i);
			
			
			switch (wildcardChar)
			{

				case '*' :
					return asterisk(test,wildcard,wildcardPos,i);
				case '?' :
					break;
				case '[' :
					if (!set(test, wildcard, wildcardPos, i,thisChar))
						return false;			
					break;			
				default :
					if (thisChar != wildcardChar)
						return false;			
			}			
			wildcardPos++;			
		}
		for (j = wildcardPos; j < wildcard.length(); j++)
		{
			if (wildcard.charAt(j) != '*')
				break;
		}
		wildcardPos = j;
		if (wildcardPos == wildcard.length())
		{
			return true;
		}  else  {
			return false;
		}
		
	}
	
	
}

