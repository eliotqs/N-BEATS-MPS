# N-BEATS-MPS
This repository includes the PyTorch and R-Studio notebooks implementing the N-BEATS-MPS deep learning architecture, developed as part of the MSc thesis "Improving forecast stability using deep learning; an extension to multiperiod forecast instability" by Yves Vanbrussel and Eliot Sodji (KU Leuven, 2022-2023). We used the Pytorch implementation of Van Belle J. as a starting point (https://github.com/KU-Leuven-LIRIS/n-beats-s).

Abstract

Forecast (in)stability refers to the variability in forecasts for a specific period in time caused by updating these forecasts as time passes and new observations become available. In the context of supply chain planning, among others, it is common practice to update forecasts on such a rolling basis, which results in both benefits and costs. Van Belle et al. (2022) proposed an extension to the state-of-the-art N-BEATS deep learning architecture for the univariate time series point forecasting problem which allows to simultaneously improve forecast stability and forecast accuracy. In this way, they showed that adding a forecast instability component to the loss function can be considered as an alternative regularization mechanism. However, their proposed extension only minimizes forecast instability with respect to adjacent forecasting origins. Therefore, we further extend their work to minimize multiperiod forecast instability, i.e. forecast instability based on adjacent and non-adjacent forecasting origins. To that end, we propose the N-BEATS- MPS deep learning architecture, which comes in two versions. We empirically show that N-BEATS-MPS can improve both multiperiod forecast stability and forecast accuracy compared to the original N-BEATS architecture, thereby enabling further improvements in the profitability of forecast updates. Furthermore, the results of N-BEATS-MPS indicate that including multiperiod forecast instability in the loss function allows to further improve the effectiveness of forecast instability as a regularization mechanism. 

Keywords: Forecast Accuracy, Multiperiod Forecast Instability, N-BEATS(-S), Regular- ization, Univariate Time Series Forecasting, Deep Learning, Supply Chain Planning


