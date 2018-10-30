import numpy as np

def DFT(vector):
    
    '''
    This function compute the Discrete Fourier Transform (DFT) for a n-dimensional array, which receives a vector as an input and return the transformed vector as output.
    
    Input: 
    vector - numpy array - vector that contains all values
    
    Output:
    tvec - numpy array - transformed vector that contains both Real and Imag parts
    '''
    
    # Converts for an array
    vector = np.asarray(vector, dtype=float)
    # Value for N that is used on the sum
    N = vector.shape[0]
    # Row and column vectors for compute the matrix
    row = np.arange(N)
    col = row.reshape((N, 1))
    # Compute the transform factor W
    W = np.exp(-2.j*np.pi*col*row/N)
    # Compute the DFT
    tvec = np.dot(W, vector)
    
    #Return the final output
    return tvec

# Auxiliar number 12
def iDFT_1(ivector):
    
    '''
    This function compute the inverse Discrete Fourier Transform (iDFT) for a n-dimensional array, which receives a vector as an input (Real and Imaginary parts) and return the inverse transformed vector as output.
    
    Input: 
    ivector - numpy array - vector that contains both Real and Imag parts
    
    Output:
    vector - numpy array - inverse transformed vector with all original values
    
    Ps. - In this function, the inverse DFT is calculated by using a simple matrix - vector product.
    '''
    
    # Converts for a complex numpy array
    ivector = np.asarray(ivector, dtype=complex)
    # Value for N that is used on the sum
    N = ivector.size
    # Row and column vectors for compute the matrix
    row = np.arange(N)
    col = row.reshape((N, 1))
    # Compute the transform factor W
    W = np.exp(2.j*np.pi*col*row/N)
    vector = np.dot(W, ivector)/N
    
    #Return the final output
    return vector    

# Auxiliar number 13
def iDFT_2(ivector):
    
    '''
    This function compute the inverse Discrete Fourier Transform (iDFT) for a n-dimensional array, which receives a vector as an input (Real and Imaginary parts) and return the inverse transformed vector as output.
    
    Input: 
    ivector - numpy array - vector that contains both Real and Imag parts
    
    Output:
    vector - numpy array - inverse transformed vector with all original values
    
    Ps. - In this function, the inverse DFT is calculated by using a simple matrix - vector product.
    '''
    
    # Converts for a complex numpy array
    ivector = np.asarray(ivector, dtype=complex)
    # Value for N that is used on the sum
    N = ivector.size
    # Row and column vectors for compute the matrix
    row = np.arange(N)
    col = row.reshape((N, 1))
    # Compute the transform factor W
    W = np.exp(-2.j*np.pi*col*row/N)
    vector = np.linalg.solve(W, ivector)
    
    #Return the final output
    return vector