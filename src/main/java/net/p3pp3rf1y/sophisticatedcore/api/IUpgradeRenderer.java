package net.p3pp3rf1y.sophisticatedcore.api;

import com.mojang.math.Vector3f;
import net.minecraft.world.level.Level;
import net.p3pp3rf1y.sophisticatedcore.renderdata.IUpgradeRenderData;

import java.util.Random;
import java.util.function.UnaryOperator;

public interface IUpgradeRenderer<T extends IUpgradeRenderData> {
	void render(Level level, Random rand, UnaryOperator<Vector3f> getPositionFromOffset, T upgradeRenderData);
}
