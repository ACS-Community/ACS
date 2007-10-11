package com.cosylab.acs.maci;

/**
 * Authentication data structure.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class AuthenticationData
{
	/**
	 * Answer to authentication question.
	 */
	private String answer;
	
	/**
	 * Client type.
	 */
	private ClientType clientType;

	/**
	 * Client implementation language.
	 */
	private ImplLang implLang;

	/**
	 * Try to reciver flag.
	 */
	private boolean recover;
	
	/**
	 * Timestamp of client start-up. Ignored if 0.
	 */
	private long timeStamp;

	/**
	 * Execution ID. Ignored if 0.
	 */
	private long executionId;

	/**
	 * Constructor.
	 * @param answer
	 * @param clientType
	 * @param implLang
	 * @param recover
	 * @param timeStamp
	 * @param executionId
	 */
	public AuthenticationData(String answer, ClientType clientType, ImplLang implLang, boolean recover, long timeStamp, long executionId) {
		this.answer = answer;
		this.clientType = clientType;
		this.implLang = implLang;
		this.recover = recover;
		this.timeStamp = timeStamp;
		this.executionId = executionId;
	}

	/**
	 * @return the answer
	 */
	public String getAnswer() {
		return answer;
	}

	/**
	 * @param answer the answer to set
	 */
	public void setAnswer(String answer) {
		this.answer = answer;
	}

	/**
	 * @return the clientType
	 */
	public ClientType getClientType() {
		return clientType;
	}

	/**
	 * @param clientType the clientType to set
	 */
	public void setClientType(ClientType clientType) {
		this.clientType = clientType;
	}

	/**
	 * @return the executionId
	 */
	public long getExecutionId() {
		return executionId;
	}

	/**
	 * @param executionId the executionId to set
	 */
	public void setExecutionId(long executionId) {
		this.executionId = executionId;
	}

	/**
	 * @return the implLang
	 */
	public ImplLang getImplLang() {
		return implLang;
	}

	/**
	 * @param implLang the implLang to set
	 */
	public void setImplLang(ImplLang implLang) {
		this.implLang = implLang;
	}

	/**
	 * @return the recover
	 */
	public boolean isRecover() {
		return recover;
	}

	/**
	 * @param recover the recover to set
	 */
	public void setRecover(boolean recover) {
		this.recover = recover;
	}

	/**
	 * @return the timeStamp
	 */
	public long getTimeStamp() {
		return timeStamp;
	}

	/**
	 * @param timeStamp the timeStamp to set
	 */
	public void setTimeStamp(long timeStamp) {
		this.timeStamp = timeStamp;
	}
	
	
}
