/*
 * @@COPYRIGHT@@
 */

package abeans.models.acs.baci.util;

/**
 * Sequence converter helper.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version		@@VERSION@@
 */
final public class SequenceConverterHelper
{
	/**
	 * Converts sequence to Abeans supported sequence.
	 * @param seq	input sequence
	 * @return		output sequence
	 */
	public static double[] toAbeansSequence(double[] seq)
	{
		return seq;
	}

	/**
	 * Converts sequence to Abeans supported sequence.
	 * @param seq	input sequence
	 * @return		output sequence
	 */
	public static double[] toAbeansSequence(float[] seq)
	{
		double[] out = new double[seq.length];
		for (int i = 0; i < out.length; i++)
			out[i] = (double)seq[i];
		return out;
	}

	/**
	 * Converts sequence to Abeans supported sequence.
	 * @param seq	input sequence
	 * @return		output sequence
	 */
	public static long[] toAbeansSequence(long[] seq)
	{
		return seq;
	}

	/**
	 * Converts sequence to Abeans supported sequence.
	 * @param seq	input sequence
	 * @return		output sequence
	 */
	public static long[] toAbeansSequence(byte[] seq)
	{
		long[] out = new long[seq.length];
		for (int i = 0; i < out.length; i++)
			out[i] = (long)seq[i];
		return out;
	}

	/**
	 * Converts sequence to Abeans supported sequence.
	 * @param seq	input sequence
	 * @return		output sequence
	 */
	public static long[] toAbeansSequence(short[] seq)
	{
		long[] out = new long[seq.length];
		for (int i = 0; i < out.length; i++)
			out[i] = (long)seq[i];
		return out;
	}

	/**
	 * Converts sequence to Abeans supported sequence.
	 * @param seq	input sequence
	 * @return		output sequence
	 */
	public static long[] toAbeansSequence(int[] seq)
	{
		long[] out = new long[seq.length];
		for (int i = 0; i < out.length; i++)
			out[i] = (long)seq[i];
		return out;
	}

	/**
	 * Converts Abesns supported sequence to requested sequence.
	 * @param seq	input sequence
	 * @return		output sequence
	 */
	public static double[] doubleSequence(double[] seq)
	{
		return seq;
	}

	/**
	 * Converts Abesns supported sequence to requested sequence.
	 * @param seq	input sequence
	 * @return		output sequence
	 */
	public static float[] floatSequence(double[] seq)
	{
		float[] out = new float[seq.length];
		for (int i = 0; i < out.length; i++)
			out[i] = (float)seq[i];
		return out;
	}

	/**
	 * Converts Abesns supported sequence to requested sequence.
	 * @param seq	input sequence
	 * @return		output sequence
	 */
	public static long[] longSequence(long[] seq)
	{
		return seq;
	}

	/**
	 * Converts Abesns supported sequence to requested sequence.
	 * @param seq	input sequence
	 * @return		output sequence
	 */
	public static byte[] byteSequence(long[] seq)
	{
		byte[] out = new byte[seq.length];
		for (int i = 0; i < out.length; i++)
			out[i] = (byte)seq[i];
		return out;
	}

	/**
	 * Converts Abesns supported sequence to requested sequence.
	 * @param seq	input sequence
	 * @return		output sequence
	 */
	public static short[] shortSequence(long[] seq)
	{
		short[] out = new short[seq.length];
		for (int i = 0; i < out.length; i++)
			out[i] = (short)seq[i];
		return out;
	}

	/**
	 * Converts Abesns supported sequence to requested sequence.
	 * @param seq	input sequence
	 * @return		output sequence
	 */
	public static int[] intSequence(long[] seq)
	{
		int[] out = new int[seq.length];
		for (int i = 0; i < out.length; i++)
			out[i] = (int)seq[i];
		return out;
	}
}
