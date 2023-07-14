clear
close all
clc

seg = load("Subject101.mat");

seg2 = seg.image;
nnz(seg2)

seg2(1:120,:,:) = [];
seg2(392:end,:,:) = [];

seg2(:,1:120,:) = [];
seg2(:,392:end,:) = [];

newimage = imbinarize(seg2, 'adaptive');

%newimage = finalimage; 

nnz(seg2)
%% Binarize Image
objcolor = 1; % object of interest color ** 0 for black ** ** 1 for white **
sz = size(newimage);

%for i = 1:sz(1)
%    for j = 1:sz(2)
%        if newimage(i,j) == 0
%            newimage(i,j) = 1;
%        end
%         if newimage(i,j) < floor(max(max(newimage))/2)
%        if mean(newmap(newimage(i,j),:)) < 0.5
%            newimage(i,j) = 0;
%        else
%            newimage(i,j) = 1;
%        end
%    end
%end

%newmap(1,:) = [0,0,0];
%newmap(2,:) = [1,1,1];
%newmap(3:end,:) = [];



if objcolor
    nagalog = newimage == 1;
else
    nagalog = newimage == 0;
end
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% Box Counting %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
recsz = 2.^(0:1:4); %vector of rectangle sizes

M = zeros(max(recsz)^2,length(recsz));

currentsize = 0;

for x = recsz % Rectangular box code
    counter = 0;
    currentsize = currentsize + 1;
    pxsz = floor(sz./x);

    numboxR = floor(sz(1) / pxsz(1));
    rowvec = [pxsz(1) * ones(1, numboxR), rem(sz(1), pxsz(1))];

    numboxC = floor(sz(2) / pxsz(2));
    colvec = [pxsz(2) * ones(1, numboxC), rem(sz(2), pxsz(2))];

    numboxH = floor(sz(3) / pxsz(3));
    heightvec = [pxsz(3) * ones(1,numboxH), rem(sz(3),pxsz(3))];

    logcell = mat2cell(nagalog,rowvec,colvec,heightvec);

    for u = 1:numboxR
        for v = 1:numboxC
            for w = 1:numboxH
            counter = counter + 1;
            temp = cell2mat(logcell(u,v,w));
            M(counter,currentsize) = sum(sum(sum(temp)));
            end
        end
    end
end


%%
%%%%%%%%%%%%%%%%%%%%%%%%%%% Set up regression %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
h = 0.1;
q = -10:h:10; %vector of q values

prbM = M./sum(M);%converting pixel counts to proportions

truesz = sz(1)./recsz;

% X = flip(log2(recsz));
X = log2(truesz);

yD = zeros(length(q),length(prbM(1,:)));
yalph = zeros(length(q),length(prbM(1,:)));
yf = zeros(length(q),length(prbM(1,:)));
mu = zeros(length(prbM(:,1)),length(prbM(1,:)),length(q));
yind = 0;


for k = q %Calculates y values
    qmoment = 0;
    yind = yind + 1;
    for a = 1:length(prbM(1,:))
        if k == 1
            yD(yind,a) = sum(nonzeros(prbM(:,a)).*log2(nonzeros(prbM(:,a))));
        else
            yD(yind,a) = log2(sum(nonzeros(prbM(:,a)).^k));
            mu(1:length(nonzeros(prbM(:,a))),a,yind) = (nonzeros(prbM(:,a)).^k)./(sum(nonzeros(prbM(:,a)).^k));
            yalph(yind,a) = sum(nonzeros(mu(:,a,yind)).*log2(nonzeros(prbM(:,a))));
            yf(yind,a) = sum(nonzeros(mu(:,a,yind)).*log2(nonzeros(mu(:,a,yind))));
        end
    end
end

Dq = zeros(length(q),1);
tauq = zeros(length(q),1);
myalpha = zeros(length(q),1);
falph = zeros(length(q),1);

fitting_func = @(p,x) p(1).*x;
options = optimset('Display','off','TolFun',1e-10);

p0=2;

%% Regression 
for currq = 1:length(q)
    if q(currq) == 1
        Dq(currq) = abs(fit(X',yD(currq,:)','poly1').p1);
    else
        tauq(currq) = (fit(X',yD(currq,:)','poly1').p1); %Calculating linear fits
        Dq(currq) = tauq(currq)./(q(currq)-1);
        myalpha(currq) = fit(X',yalph(currq,:)','poly1').p1;
        falph(currq) = fit(X',yf(currq,:)','poly1').p1;
    end
end
%Legandre Transformation
alphaleg = zeros(length(tauq),1);
alphaleg(1) = (tauq(2) - tauq(1))/h;
alphaleg(end) = (tauq(end) - tauq(end-1))/h;

for step = 2:length(alphaleg)-1
    alphaleg(step) = (tauq(step+1) - tauq(step-1))/(2*h);
end

fleg = q'.*alphaleg - tauq;

%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% Plot Dq vs q and Spectra %%%%%%%%%%%%%%%%%%%%%%%%%%%
figure(1)
plot(q,Dq)
xlabel('q')
ylabel('Dq')
title('Dq vs q')
%ylim([0 3])
%saveas(gcf,fullfile(savefolder,filenamedqq),'jpeg')

figure(2)
hold on
grid on
scatter(myalpha,falph,'.r')
scatter(alphaleg,fleg,'.b')
xlabel('$\alpha$','Interpreter','latex')
ylabel('f($\alpha$)',Interpreter='latex')
title('Multifractal Spectrum')
hold off
%saveas(gcf,fullfile(savefolder,filenamespectrum),'jpeg')
max(falph)
max(fleg)
% figure(10)
% hold on
% scatter(-myalpha,-falph,'b','MarkerEdgeAlpha',0.5)
% xlabel('alpha')
% ylabel('f(alpha)')
% title('Multifractal Spectrum')
% hold off