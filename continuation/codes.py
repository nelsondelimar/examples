import numpy as np # Numpy library

def deg_rad(angle):
    
    '''
    This function converts an angle value in degrees to an another value in radian.
    
    Input:
    angle - float - angle in degrees
    
    Output:
    argument - float - angle in radian    
    '''
    
    # Condition for the calculation
    if angle > 360.:
        r = angle//360
        angle = angle - r*360
    
    # Angle conversion
    argument = (angle/180.)*np.pi
    
    # Return the final output
    return argument

def rad_deg(argument):
    
    '''
    This function converts an angle value in radian to an another value in degrees.
    
    Input:
    argument - float - angle in radian
        
    Output:
    angle - float - angle in degrees    
    '''
    
    # Check the input value for an angle
    assert type(argument) is float, 'Angle value must be decimal!'
    # Angle conversion
    angle = (argument/np.pi)*180.
    # Return the final output
    return angle

def dir_cossine(theta_inc, theta_dec, theta_azi):
    
    '''
    This function calculates the cossines projected values on directions using inclination and declination values. Here, we do not considerate an azimuth as a zero value, but as a input value.
    
    Inputs:
    theta_inc - inclination angle
    theta_dec - declination angle
    theta_azi - azimuth angle
    
    Outputs:
    dirA - projected cossine A
    dirB - projected cossine B
    dirC - projected cossine C    
    '''
    
    # Use the function to convert some values
    incl = deg_rad(theta_inc)
    decl = deg_rad(theta_dec)
    azim = deg_rad(theta_azi)
    
    # Calculates the projected cossine values
    dirA = np.cos(incl)*np.cos(decl - azim)
    dirB = np.cos(incl)*np.sin(decl - azim)
    dirC = np.sin(incl)
    
    # Return the final output
    return dirA, dirB, dirC

def regional(field_values):
    
    '''
    This fucntion computes the projected components of the regional magnetic field in all directions X, Y and Z. This calculation is done by using a cossine projected function, which recieves the values for an inclination, declination and also and azimuth value. It returns all three components for a magnetic field (Fx, Fy e Fz), using a value for the regional field (F) as a reference for the calculation.
    
    Inputs: 
    valF - float - regional magnetic field value
    incF - float - magnetic field inclination value
    decF - float - magnetic field declination value
    aziF - float - magnetic field azimuth value
    
    Outputs:
    vecF - numpy array - F componentes along X, Y e Z axis
        
    Ps. All inputs can be arrays when they are used for a set of values.    
    '''
    
    assert field_values[0] != 0., 'Value of the regional magnetic field must be nonzero!'
        
    # Computes the projected cossine
    X, Y, Z = dir_cossine(field_values[1], field_values[2], field_values[3])
    # Compute all components
    Fx = field_values[0]*X
    Fy = field_values[0]*Y
    Fz = field_values[0]*Z
    # Set the F values as an array
    vecF =[Fx, Fy, Fz]
    # Return the final output
    return vecF

def prism_tfa(x, y, z, prism, directions, field):
    
    '''
    This function calculates the total field anomaly produced by a rectangular prism located under surface; it is a Python implementation for the Subroutin MBox which is contained on Blakely (1995). It recieves: the coordinates of the positions in all directions, the elements of the prims, the angle directions and the elements of the field. That function also uses the auxilary function DIR_COSSINE to calculate the projections due to the field F and the source S.
    
    Inputs:
    x, y - numpy arrays - observation points in x and y directions
    z - numpy array/float - height for the observation
    prism - numpy array - all elements for the prims
        prism[0, 1] - initial and final coordinates at X (dimension at X axis!)
        prism[2, 3] - initial and final coordinates at Y (dimension at Y axis!)
        prism[4, 5] - initial and final coordinates at Z (dimension at Z axis!)
        prism[6] - magnetic intensity
        
    Output:
    tfa - numpy array - calculated total field anomaly
    
    X and Y represents North and East; Z is positive downward.
    Ps. Z can be a array with all elements for toppography or a float point as a flight height (for example!).
    '''    
    
    # Stablish some constants
    t2nt = 1.e9 # Testa to nT - conversion
    cm = 1.e-7  # Magnetization constant

    # Calculate the directions for the source magnetization and for the field
    Ma, Mb, Mc = dir_cossine(directions[0], directions[1], directions[2]) # s -> source
    Fa, Fb, Fc = dir_cossine(field[0], field[1], field[2]) # f -> field

    # Aranges all values as a vector
    MF = np.array([Ma*Fb + Mb*Fa, 
                   Ma*Fc + Mc*Fa, 
                   Mb*Fc + Mc*Fb, 
                   Ma*Fa, 
                   Mb*Fb, 
                   Mc*Fc])
    
    # Limits for initial and final position along the directions
    A = [prism[0] - x, prism[1] - x]
    B = [prism[2] - y, prism[3] - y]
    H = [prism[5] - z, prism[4] - z]
    
    # Create the zero array to allocate the total field result
    tfa = np.zeros_like(x)
    
    # Loop for controling the signal of the function    
    mag = prism[6]
    for k in range(2):
        mag *= -1
        H2 = H[k]**2
        for j in range(2):
            Y2 = B[j]**2
            for i in range(2):
                X2 = A[i]**2
                AxB = A[i]*B[j]
                R2 = X2 + Y2 + H2
                R = np.sqrt(R2)
                HxR = H[k]*R
                tfa += ((-1.)**(i + j))*mag*(0.5*(MF[2])*np.log((R - A[i])/(R + A[i])) + 0.5*(MF[1])*
                                             np.log((R - B[j])/(R + B[j])) - (MF[0])*np.log(R + H[k]) -
                                             (MF[3])*np.arctan2(AxB, X2 + HxR + H2) -
                                             (MF[4])*np.arctan2(AxB, R2 + HxR - X2) +
                                             (MF[5])*np.arctan2(AxB, HxR))        
        
        tfa *= t2nt*cm
        
    # Return the final output
    return tfa

def prism_gz(xo, yo, zo, prism):
    
    '''
    This function is a Python implementation for the vertical component for the gravity field due to a rectangular prism, which has initial and final positions equals to xi and xf, yi and yf, for the X and Y directions. This function also recieve the obsevation points for an array or a grid and also the value for height of the observation, which can be a simple float number (as a level value) or a 1D array.
    
    Inputs:
    x, y - numpy arrays - observation points in x and y directions
    z - numpy array/float - height for the observation
    prism - numpy array - all elements for the prims
        prism[0, 1] - initial and final coordinates at X (dimension at X axis!)
        prism[2, 3] - initial and final coordinates at Y (dimension at Y axis!)
        prism[4, 5] - initial and final coordinates at Z (dimension at Z axis!)
        prism[6] - density value
        
    Output:
    gz - numpy array - vertical component for the gravity atraction
    
    '''
    
    # Definitions for all distances
    x = [prism[1] - xo, prism[0] - xo]
    y = [prism[3] - yo, prism[2] - yo]
    z = [prism[5] - zo, prism[4] - zo]
    
    # Definition for density
    rho = prism[6]
    
    # Definition - some constants
    G = 6.673e-11
    si2mGal = 10.e5
    
    # Numpy zeros array to update the result
    gz = np.zeros_like(xo)
    
    # Compute the value for Gz
    for k in range(2):
        for j in range(2):
            for i in range(2):
                r = np.sqrt(x[i]**2 + y[j]**2 + z[k]**2)
                result = -(x[i]*np.log(y[j] + r) + y[j]*np.log(x[i] + r) - z[k]*np.arctan2(x[i]*y[j], z[k]*r))
                gz += ((-1.)**(i + j + k))*result*rho
                
    # Multiplication for all constants and conversion to mGal
    gz *= G*si2mGal
    
    # Return the final output
    return gz
