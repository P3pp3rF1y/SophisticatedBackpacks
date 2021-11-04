package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.minecraft.util.math.vector.Vector3d;
import net.minecraft.world.World;

import java.util.Random;
import java.util.function.UnaryOperator;

public interface IUpgradeRenderer<T extends IUpgradeRenderData> {
	void render(World world, Random rand, UnaryOperator<Vector3d> getPositionFromOffset, T upgradeRenderData);
}
